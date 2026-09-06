import os
import shutil
import logging
import logging.config
from time import time
from multiprocessing import Pool
import datetime
import tensorflow as tf
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from PIL import Image
import pandas as pd
import json
import platform
import sys
import importlib
from logging.handlers import TimedRotatingFileHandler

from .functions import *

def load_model(config, num_classes):
    if int(config['training']['start']) > 0:
        return(tf.keras.models.load_model(config['training']['model_path'] + '/' + config['training']['model_name'] + '.keras'))
    
    # Pass 'config' into init_model so it knows which architecture to load
    return(init_model(config, num_classes, int(config['training']['image_size']), int(config['training']['image_size'])))


def init_model(config, num_classes, img_height, img_width):
    # 1. Grab the exact model name from your config.json (e.g., 'ResNet152')
    model_name = config['training']['model_type']
    
    # 2. Dynamically import the corresponding .py file from the current directory
    try:
        model_module = importlib.import_module(f".{model_name}", package=__package__)
        Model = getattr(model_module, 'Model')
    except ImportError:
        raise ValueError(f"Could not find {model_name}.py in the dpi_fun directory. Check config.json.")
    except AttributeError:
        raise ValueError(f"The file {model_name}.py does not contain a function named 'Model'.")
    
    # 3. Stabilize training with gradient clipping
    optimizer = tf.keras.optimizers.Adam(learning_rate=1e-4, clipnorm=1.0)
    
    # Initialize your custom architecture
    model_obj = Model([img_height, img_width, 1], num_classes)
    
    model_obj.compile(
        optimizer=optimizer, 
        loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=False), 
        metrics=['accuracy']
    )
    model_obj.summary()
    return(model_obj)


def train_model(model, config, train_ds, val_ds, devices):
    csv_logger = tf.keras.callbacks.CSVLogger(
        config['training']['model_path'] + '/' + config['training']['model_name'] + '.log',
         append = False,
          separator = ','
    )
    
    checkpoint_callback = tf.keras.callbacks.ModelCheckpoint(
        filepath = config['training']['model_path'] + os.path.sep + config['training']['model_name'] + '_checkpoint.keras',
        save_best_only = True,
        monitor = 'val_loss',
        mode = 'min',
        save_weights_only = False,
        save_freq = 'epoch' # Changed from 5 to 'epoch' to ensure validation metrics align
    )

    # Added: Automatically reduce learning rate when validation loss plateaus
    lr_scheduler = tf.keras.callbacks.ReduceLROnPlateau(
        monitor='val_loss',
        factor=0.5,
        patience=5,
        min_lr=1e-6,
        verbose=1
    )

    # Added: Stop training safely if validation loss stops improving
    early_stopping = tf.keras.callbacks.EarlyStopping(
        monitor='val_loss',
        patience=20,
        restore_best_weights=True,
        verbose=1
    )

<<<<<<< Updated upstream
    #with tf.device(devices):
    history = model.fit(
        train_ds,
        validation_data=val_ds,
        epochs=int(config['training']['stop'])-int(config['training']['start']),
        initial_epoch=int(config['training']['start']),
        batch_size = int(config['training']['batchsize']),
        # Added missing callbacks to the training loop
        callbacks=[csv_logger, checkpoint_callback, lr_scheduler, early_stopping]
    )
=======
    with tf.device('/GPU:0'):
        history = model.fit(train_ds,
                        validation_data=val_ds,
                        epochs=int(config['training']['stop'])-int(config['training']['start']),
                        initial_epoch=int(config['training']['start']),
                        batch_size = int(config['training']['batchsize']),
                        callbacks=[csv_logger])
>>>>>>> Stashed changes
    
    return(model, history)


def init_ts(config):
    train_ds, val_ds = tf.keras.utils.image_dataset_from_directory(
        config['training']['scnn_dir'],
        interpolation='area',
        validation_split = config['training']['validationSetRatio'],
        subset = "both",
        seed = int(config['training']['seed']),
        image_size = (int(config['training']['image_size']), int(config['training']['image_size'])),
        batch_size = int(config['training']['batchsize']),
        color_mode = 'grayscale')
    
    return(train_ds, val_ds)


def getTensorflowDevices(logger):
    devices = tf.config.list_physical_devices()
    gpus = tf.config.list_physical_devices('GPU')

    if gpus:
        logger.info(f"Found {len(gpus)} Tensorflow-compatible GPUs.")
        for idx, g in enumerate(gpus):
            logger.debug(f"Device {idx}: {g}")
        return gpus
    else:
        logger.warn(f"No (compatible) GPUs found, defaulting to CPU execution (n={len(devices)}).")
        for idx, cpu in enumerate(devices):
            logger.debug(f"Device {idx}: {cpu}")
    return [x.name for x in devices]


def mainTrain(config, logger):

    v_string = "V2024.05.22"
    session_id = str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S")).replace(':', '')
    
    logger.info(f"Starting CNN Model Training Script {v_string}")
    logger.debug(f"Session ID is {session_id}.")
    timer = {'init' : time()}

    deviceList = getTensorflowDevices(logger)

    # ## Load training and validation data sets
    train_ds, val_ds = init_ts(config)
    logger.info('Loaded training and validation datasets.')
    logger.debug(f"Datasets have {len(train_ds.class_names)} categories.")

    logger.debug('Attempting to load or initialize model.')
    timer['model_load_start'] = time()
    model = load_model(config, len(train_ds.class_names))
    timer['model_load_end'] = time()
    logger.debug('Model loaded successfully.')

    logger.info('Starting to train model.')
    
    ## Train model
    timer['model_train_start'] = time()
    model, history = train_model(model, config, train_ds, val_ds, deviceList)
    timer['model_train_end'] = time()
    logger.info('Model trained. Running post-processing steps.')

    # Extract true validation labels AFTER training completes
    y = np.concatenate([y for x, y in val_ds], axis = 0)

    ## Post training steps
    model_save_pathname = config['training']['model_path'] + os.path.sep + config['training']['model_name'] + '.keras'
    json_save_pathname = config['training']['model_path'] + os.path.sep + config['training']['model_name'] + '.json'

    if os.path.exists(model_save_pathname):
        if config['training']['overwrite']:
            logger.info(f"Saved keras file exists for {config['training']['model_name']}. Overwrite is enabled so existing file is being removed.")
            delete_file(model_save_pathname, logger)
            logger.debug(f"Deleted file {model_save_pathname}.")
            if os.path.exists(json_save_pathname):
                delete_file(json_save_pathname, logger)
                logger.debug(f"Deleted file {json_save_pathname}.")
        else :
            config['training']['model_name'] = config['training']['model_name'] + '-' + session_id ## Append session ID to model_name from here on out!
            logger.warn(f"Saved keras file exists. Overwrite is not indicated so current model will be saved as {config['training']['model_name'] + '.keras'}.")
            model_save_pathname = config['training']['model_path'] + os.path.sep + config['training']['model_name'] + '.keras'
            config['training']['model_path'] + os.path.sep + config['training']['model_name'] + '.json'
    
    logger.debug(f"Saving model keras file to {config['training']['model_path']}.")
    timer['model_save_start'] = time()
    model.save(model_save_pathname)
    timer['model_save_end'] = time()
    logger.debug(f"Model saved to {config['training']['model_name'] + '.keras'}.")
    
    logger.debug('Running preditions on validation dataset.')
    predictions = model.predict(val_ds)
    prediction_matrix = pd.DataFrame(predictions, index = y, columns = train_ds.class_names)
    prediction_matrix.to_csv(config['training']['model_path'] + os.path.sep + config['training']['model_name'] + ' predictions.csv')
    
    logger.debug('Developing confusion matrix from validation dataset.')
    predictions = np.argmax(predictions, axis = -1)
    confusion_matrix = tf.math.confusion_matrix(y, predictions)
    confusion_matrix = pd.DataFrame(confusion_matrix, index = train_ds.class_names, columns = train_ds.class_names)
    confusion_matrix.to_csv(config['training']['model_path'] + os.path.sep + config['training']['model_name'] + ' confusion.csv')
    logger.debug(f"Confusion matrix saved to {config['training']['model_path'] + os.path.sep + config['training']['model_name'] + ' confusion.csv'}")
    
    timer['close'] = time()
    
    logger.debug('Generating model saidecar.')
    ## Generate sidecar dictionary:
    sidecar = {
        'model_name' : config['training']['model_name'],
        'model_type' : config['training']['model_type'],
        'labels' : train_ds.class_names,
        'script_version' : v_string,
        'sessionid': str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S")).replace(':', ''),
        'config' : config,
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
    
    logger.debug(f"Writing model sidecar to {json_save_pathname}.")
    json_object = json.dumps(sidecar, indent=4)
    with open(json_save_pathname, "w") as outfile:
        outfile.write(json_object)
        logger.debug("Sidecar writting finished.")
    
    logger.debug('Training finished.')
    sys.exit(0) # Successful close