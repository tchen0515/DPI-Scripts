#!/usr/bin/env python3
"""Segmentation script for DPI-Scripts

Usage:
    ./segmentation.py <dir>

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
import subprocess
import getpass
from dpi_fun import *
#from dpi_fun.functions import *
#from dpi_fun.functionsSegmentation import *


if __name__ == "__main__":
    """
    Entrypoint for script when run from the command line.
    """
    if not os.path.exists('config.json'):
        print(f"Required configuration file 'config.json' not found. Aborting.")
        sys.exit(1)
    
    with open('config.json', 'r') as f:
        config = json.load(f)

    logger = setup_logger('Segmentation (Main)', config)

    ## Setup directories
    directory = sys.argv[-1]
    config['raw_dir'] = os.path.abspath(directory)
    config['segmentation_dir'] = constructSegmentationDir(config['raw_dir'], config)
    _, filename = os.path.split(config['raw_dir'])
    config['identity'] = filename

    ## Find files to process:
    avis = findVideos(config, logger)
    imgsets = findImgsets(config, logger)

    ## Run segmentation
    sidecar = mainSegmentation(config, logger, avis, imgsets)

    ## Write sidecar file
    json_save_pathname = config['segmentation_dir'] + '.json'
    json_object = json.dumps(sidecar, indent=4)
    with open(json_save_pathname, "w") as outfile:
        outfile.write(json_object)
        logger.debug("Sidecar writting finished.")
        
    ## --- NEW CLEANUP CODE ---
    logger.info("Task complete. Running scratch directory cleanup...")
    
    # Get the scratch dir from config, default to /dev/shm if not found
    try:
        scratch_dir = config['segmentation']['scratch_dir']
    except KeyError:
        scratch_dir = '/dev/shm/tchen/'

    # Get the current username (e.g., 'tchen')
    current_user = getpass.getuser() 

    # Construct the find command you provided earlier
    cleanup_command = ['find', scratch_dir, '-user', current_user, '-type', 'f', '-delete']
    
    try:
        # Run the command
        subprocess.run(cleanup_command, check=True)
        logger.info(f"Successfully cleaned up files in {scratch_dir} for user {current_user}.")
    except subprocess.CalledProcessError as e:
        logger.error(f"Failed to clean up scratch directory: {e}")
    except Exception as e:
        logger.error(f"An unexpected error occurred during cleanup: {e}")
    ## ------------------------

    sys.exit(0) # Successful close