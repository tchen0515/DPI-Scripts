#!/usr/bin/env python3
"""DenseNet training script for DPI-Scripts

Usage:
    ./train.py

License:
    MIT License

    Copyright (c) 2023 Thomas Kelly, modified by Tz-Chian Chen 2026

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
import importlib
import builtins
from dpi_fun import *

if __name__ == "__main__":
    # 1. Read the config file first
    with open('config.json', 'r') as f:
        config = json.load(f)

    # 2. Get the model_type from the config (e.g., "DenseNet121" or "ConvNeXt")
    model_type = config['training']['model_type']
    
    print(f"Dynamically loading architecture: dpi_fun.{model_type}")
    
    # 3. Dynamically import the chosen architecture file
    model_module = importlib.import_module(f'dpi_fun.{model_type}')
    
    # 4. Inject the Model function into Python's built-in namespace 
    # This ensures your functionsTrain.py script can "see" the Model() function
    # exactly as it did when you were using 'from dpi_fun.DenseNet121 import *'
    builtins.Model = model_module.Model

    # 5. Proceed with your existing pipeline
    logger = setup_logger('Training (main)', config)
    mainTrain(config, logger)