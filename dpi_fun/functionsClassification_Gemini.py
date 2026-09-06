import os
import sys
import shutil
import logging
import logging.config
import datetime
from time import time
from multiprocessing import Pool, Queue
import tensorflow as tf
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image
import os
import pandas as pd
import json
from logging.handlers import TimedRotatingFileHandler
from multiprocessing import Process
import platform
from .functions import *


def loadModel(config, logger):
    """
    Helper function to load model and sidecar. 
    """
    model_path = config['classification']['model_dir'] + os.path.sep + config['classification']['model_name'] + ".keras"
    label_path = config['classification']['model_dir'] + os.path.sep + config['classification']['model_name'] + ".json"
    logger.info(f"Loading model from {model_path}.")
    logger.info(f"Loading model sidecar from {label_path}.")
    model = tf.keras.models.load_model(model_path)

    if config['classification']['feature_space']:
        ## Modify model for feature extraction:
        # Remove final softmax activation to expose penultimate dense layer. Generate as new model object.
        x = model.layers[-3].output 
        model = tf.keras.models.Model(inputs = model.input, outputs = x)
    
    with open(label_path, 'r') as file:
        sidecar = json.load(file)
        logger.debug('Sidecar Loaded.')

    logger.info(f"Loaded keras model {config['classification']['model_name']} and sidecar JSON file.")

    return model, sidecar


def mainClassifcation(config, logger):
    """
    Main function for classification.
    """
    v_string = "V2024.05.22"
    #session_id = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S")).replace(':', '')
    logger.info(f"Starting Plankline Classification Script {v_string}")
    timer = {'init' : time()}

    if not os.path.exists(config['segmentation_dir']):
        logger.error(f'Specified path ({config["segmentation_dir"]}) does not exist. Stopping.')
        sys.exit(1)

    if config['classification']['cpuonly']:
        logger.info("Disabling GPU support for this Tensorflow session.")
        try:
            # Disable all GPUS
            tf.config.set_visible_devices([], 'GPU')
            visible_devices = tf.config.get_visible_devices()
            for device in visible_devices:
                assert device.device_type != 'GPU'
        except:
            logger.info("Unable to disable GPU device. Exception thrown, continuing.")
            pass
    
    # Load model
    timer['model_load_start'] = time()
    model, sidecar = loadModel(config, logger)
    timer['model_load_end'] = time()

    # ### Setup Folders and run classification on each segment output
    timer['folder_prepare_start'] = time()

    logger.debug(f"Segmentation directory is {config['segmentation_dir']}.")
    logger.debug(f"Classification directory is {config['classification_dir']}.")
    
    try:
        os.makedirs(config['classification_dir'], int(config['general']['dir_permissions']), exist_ok = True)
    except PermissionError:
        logger.error(f"Permission denied: Unable to create classification directory '{config['classification_dir']}'.")
    except OSError as e:
        logger.error(f"Error creating directory '{config['classification_dir']}': {e}")
    
    root = [z for z in os.listdir(config['segmentation_dir']) if z.endswith('.zip')]
        
    logger.info(f"Found {len(root)} archives for potential processing.")
    for idx, r in enumerate(root):
        logger.debug(f"Archive {idx}: {r}")
    timer['folder_prepare_end'] = time()

    n = 1
    init_time = time()
    timer['processing_start'] = time()
    for r in root:
        start_time = time()
        runClassifier(r, model, sidecar, config, logger)
        # Note: cleanup is now handled inside runClassifier
        end_time = time()
        logger.info(f"Processed {n} of {len(root)} files.\t\t Iteration: {end_time-start_time:.2f} seconds\t Estimated remainder: {(end_time - init_time)/n*(len(root)-n) / 60:.1f} minutes.\t Elapsed time: {(end_time - init_time)/60:.1f} minutes.")
        n+=1
            
    logger.info(f"Finished classification. Total time: {(end_time - init_time)/60:.1f} minutes.")
    timer['processing_end'] = time()

    classificationSidecar = {
        'directory' : config['segmentation_dir'],
        'nFiles' : len(root),
        'script_version' : v_string,
        'sessionid': str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S")).replace(':', ''),
        'config' : config,
        'labels' : sidecar['labels'],
        'system_info' : {
            'System' : platform.system(),
            'Node' : platform.node(),
            'Release' : platform.release(),
            'Version' : platform.version(),
            'Machine' : platform.machine(),
            'Processor' : platform.processor()
        },
        'timings' : timer
    }

    return classificationSidecar


def runClassifier(r, model, sidecar, config, logger):
    if not is_file_above_minimum_size(config['segmentation_dir'] + os.path.sep + r, 128, logger):
        logger.warn(f"File {r} either does not exist or does not meet minimum size requirements (128 B). Skipping to next file.")
        return(1)

    ##Unpack zip file
    r2 = r.replace(".zip", "")
    zip_source = os.path.join(config['segmentation_dir'], r)

    # === NFS OPTIMIZATION: Use Scratch Directory if Enabled ===
    use_scratch = config['segmentation'].get('use_scratch', False)
    if use_scratch:
        scratch_dir = config['segmentation'].get('scratch_dir', '/tmp')
        # Create a specific folder in scratch for this file
        unpack_path = os.path.join(scratch_dir, r2)
        os.makedirs(unpack_path, exist_ok=True)
    else:
        # Fallback to unpacking in the source directory (NFS)
        unpack_path = os.path.join(config['segmentation_dir'], r2)
    # ==========================================================

    classification_output_filepath = config['classification_dir'] + os.path.sep + r2 + ' prediction.csv'
    if os.path.exists(classification_output_filepath):
        if config['classification']['overwrite']:
            logger.info(f"classification file exists for {r2} and overwrite is enabled. Overwritting prior classification file.")
            delete_file(classification_output_filepath, logger)
        else:
            logger.info(f"classification file exists for {r2}, overwrite is contraindicated in the config file. Skipping.")
            return

    logger.debug(f"Unpacking archive at {zip_source} to destination {unpack_path}.")
    try:
        shutil.unpack_archive(zip_source, unpack_path, 'zip')
    except Exception as e:
        logger.error(f"Failed to unpack {r}: {e}")
        # Cleanup partial unpack if on scratch
        if use_scratch and os.path.exists(unpack_path):
            shutil.rmtree(unpack_path, ignore_errors=True)
        return

    ## Load and preprocess images
    images = []
    image_files = []
    valid_extensions = tuple(config['classification']['image_extensions'])
    logger.debug(f"Valid extensions: {', '.join(valid_extensions)}")

    try:
        # Iterate over the UNPACKED directory
        for img in os.listdir(unpack_path):
            if img.endswith(valid_extensions): 
                # logger.debug(f"Loading image {img}.")
                image_files.append(img)
                img_full_path = os.path.join(unpack_path, img)
                img_data = tf.keras.preprocessing.image.load_img(img_full_path,
                            target_size = (int(config['classification']['image_size']), int(config['classification']['image_size'])),
                            color_mode = 'grayscale')
                img_data = np.expand_dims(img_data, axis = 0)
                images.append(img_data)
            else :
                # logger.debug(f"Skipping file {img}.")
                pass
        
        if len(images) > 0:
            images = np.vstack(images)
            logger.debug("Finished loading images. Starting prediction.")
            predictions = model.predict(images, verbose = 0)

            df = pd.DataFrame(predictions, index = image_files) 
            if not config['classification']['feature_space']: 
                df.columns = sidecar['labels']
            df.to_csv(classification_output_filepath, index = True, header = True, sep = ',')
        else:
            logger.warn(f"No images found in archive {r}.")

    except Exception as e:
        logger.error(f"Error classifying {r}: {e}")

    finally:
        # === CLEANUP ===
        # Always remove the unpacked folder to keep scratch/NFS clean
        # Using shutil.rmtree which handles directories (unlike delete_file potentially)
        if os.path.exists(unpack_path):
            try:
                shutil.rmtree(unpack_path)
                logger.debug(f"Cleaned up {unpack_path}")
            except Exception as e:
                logger.error(f"Failed to cleanup {unpack_path}: {e}")
