#!/usr/bin/env python3
"""DenseNet training script for DPI-Scripts

Usage:
    ./train.py

License:
    MIT License

    Copyright (c) 2023 Thomas Kelly

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
"""

import json
from dpi_fun import *
import tensorflow as tf # <-- ADD THIS HERE (Around Row 35)

if __name__ == "__main__":
    with open('config.json', 'r') as f:
        config = json.load(f)

    logger = setup_logger('Training (main)', config)
    
    # <-- ADD THIS ENTIRE BLOCK HERE (Around Row 41) -->
    if config['training'].get('use_mixed_precision', False):
        logger.info("Enabling Mixed Precision (float16)")
        tf.keras.mixed_precision.set_global_policy('mixed_float16')
    else:
        logger.info("Using standard 32-bit precision")
    # <------------------------------------------------>

    mainTrain(config, logger)