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
from sklearn.metrics import fbeta_score, f1_score   # for threshold function. F1 score for evaluating loss weight scaling (tau)
from sklearn.utils.class_weight import compute_class_weight
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

# --- NEW HELPER FUNCTION ---   11/10/26
def compute_balanced_weights(train_ds, tau):
    """Calculates class weights using a power-law distribution based on tau."""
    class_indices = []
    for images, labels in train_ds.unbatch():
        class_indices.append(labels.numpy())

    class_indices = np.array(class_indices)
    classes = np.unique(class_indices)

    # Calculate occurrences of each class
    counts = np.bincount(class_indices)

    # Apply power law formula: w_i = (n_i + 1)^(-tau)
    weights = (counts[classes] + 1) ** (-float(tau))

    # Convert to dictionary format required by Keras
    class_weight_dict = {cls: weight for cls, weight in zip(classes, weights)}
    return class_weight_dict
# ---------------------------

# Record Macro F1 score for loss weight scaling evaluation
class ValMacroF1Callback(tf.keras.callbacks.Callback):
    def __init__(self, val_ds):
        super().__init__()
        self.val_ds = val_ds
        # Pre-extract true labels once to speed up epoch transitions
        self.y_true = np.concatenate([y for x, y in val_ds], axis=0)

    def on_epoch_end(self, epoch, logs=None):
        # Generate predictions for the validation set
        y_pred_probs = self.model.predict(self.val_ds, verbose=0)
        y_pred = np.argmax(y_pred_probs, axis=-1)
        
        # Calculate Macro F1
        macro_f1 = f1_score(self.y_true, y_pred, average='macro', zero_division=0)
        
        # Add to logs so CSVLogger saves it
        logs = logs or {}
        logs['val_macro_f1'] = macro_f1
        print(f" - val_macro_f1: {macro_f1:.4f}")
#---------------------------------------------------

def train_model(model, config, train_ds, val_ds, devices):
    csv_logger = tf.keras.callbacks.CSVLogger(
        config['training']['model_path'] + '/' + config['training']['model_name'] + '.log',
         append = False,
          separator = ','
    )
    
    checkpoint_callback = tf.keras.callbacks.ModelCheckpoint(
        filepath = config['training']['model_path'] + os.path.sep + config['training']['model_name'] + '_checkpoint.keras',
        save_best_only = True,
        monitor = 'val_macro_f1',  # <-- MODIFIED
        mode = 'max',              # <-- MODIFIED
        save_weights_only = False,
        save_freq = 'epoch' 
    )

    lr_scheduler = tf.keras.callbacks.ReduceLROnPlateau(
        monitor='val_macro_f1',    # <-- MODIFIED
        mode='max',
        factor=0.5,
        patience=5,
        min_lr=1e-6,
        verbose=1
    )

    early_stopping = tf.keras.callbacks.EarlyStopping(
        monitor='val_macro_f1',    # <-- MODIFIED
        mode='max',                # <-- ADDED
        patience=20,
        restore_best_weights=True,
        verbose=1
    )

    # --- ADDED: Compute class weights here ---
    print("Computing class weights to handle class imbalance...")
    class_weight_dict = compute_balanced_weights(train_ds, config['training']['tau'])
    # Macro F1 score
    f1_callback = ValMacroF1Callback(val_ds)

    history = model.fit(
        train_ds,
        validation_data=val_ds,
        epochs=int(config['training']['stop'])-int(config['training']['start']),
        initial_epoch=int(config['training']['start']),
        batch_size = int(config['training']['batchsize']),
        class_weight=class_weight_dict,  # <-- ADDED: Pass class weights to fit
        # --- MODIFIED: Insert f1_callback BEFORE csv_logger ---
        callbacks=[f1_callback, csv_logger, checkpoint_callback, lr_scheduler, early_stopping]
    )
    
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

# add calibrate_thresholds
def calibrate_f2_thresholds(y_true, predictions, class_names, logger):
    logger.debug('Calibrating F2-score thresholds for target taxa.')
    optimal_thresholds = {}
    
    for c_idx, class_name in enumerate(class_names):
        best_thresh = 0.5
        best_f2 = 0.0
        
        # Test candidate thresholds from 0.10 to 0.90
        for thresh in np.arange(0.10, 0.90, 0.05):
            y_true_binary = (y_true == c_idx).astype(int)
            y_pred_binary = (predictions[:, c_idx] >= thresh).astype(int)
            
            score = fbeta_score(y_true_binary, y_pred_binary, beta=2, zero_division=0)
            if score > best_f2:
                best_f2 = score
                best_thresh = thresh
                
        optimal_thresholds[class_name] = round(float(best_thresh), 2)
        logger.debug(f"Calibrated {class_name}: Threshold {optimal_thresholds[class_name]} (F2: {best_f2:.4f})")
        
    return optimal_thresholds

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
    
    # --- NEW CALIBRATION STEP ---
    # Pass the raw probabilities (predictions) and true labels (y) to find thresholds
    calibrated_thresholds = calibrate_f2_thresholds(y, predictions, train_ds.class_names, logger)
    # ----------------------------
    
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
        'model_name' : config['training']['model_name'], #[cite: 6]
        'model_type' : config['training']['model_type'], #[cite: 6]
        'labels' : train_ds.class_names, #[cite: 6]
        'thresholds' : calibrated_thresholds,  # <--- NEW KEY ADDED HERE
        'script_version' : v_string, #[cite: 6]
        'sessionid': str(datetime.datetime.now().strftime("%Y%m%d_%H%M%S")).replace(':', ''), #[cite: 6]
        'config' : config, #[cite: 6]
        'system_info' : { #[cite: 6]
            # ... existing system info ...
        },
        'timings' : timer #[cite: 6]
    }
    
    logger.debug(f"Writing model sidecar to {json_save_pathname}.")
    json_object = json.dumps(sidecar, indent=4)
    with open(json_save_pathname, "w") as outfile:
        outfile.write(json_object)
        logger.debug("Sidecar writting finished.")
    
    logger.debug('Training finished.')
    sys.exit(0) # Successful close