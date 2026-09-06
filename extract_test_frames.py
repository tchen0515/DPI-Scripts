#!/usr/bin/env python3
"""
Extract Test Frames for Unlabeled Video Inspection
Customized for local DPI-Scripts environment.
"""

import os
import sys
import json
import cv2
import shutil
import getpass
import subprocess
import concurrent.futures
from time import time
from dpi_fun.functions import *
from dpi_fun.functionsSegmentation import *

def extract_video_frames(avi_path, segmentation_dir, config):
    """
    Worker function to extract subsampled frames from an unlabeled video file.
    """
    logger = setup_logger('Frame Extraction (Worker)', config)
    _, filename = os.path.split(avi_path)
    
    # Custom output labeling: append -TEST_EXTRACT to output folders and files
    output_path = os.path.join(segmentation_dir, f"{filename}-TEST_EXTRACT")
    scratch_path = get_scratch(config, f"{filename}-TEST_EXTRACT")

    # Check if archive already exists
    zip_archive = os.path.join(segmentation_dir, f"{filename}-TEST_EXTRACT.zip")
    if is_file_above_minimum_size(zip_archive, 0, logger):
        if not config['segmentation'].get('overwrite', False):
            logger.info(f"Test extract archive already exists for {filename}. Skipping.")
            return
        delete_file(zip_archive, logger)

    os.makedirs(scratch_path, exist_ok=True)
    os.makedirs(output_path, exist_ok=True)

    video = cv2.VideoCapture(avi_path)
    if not video.isOpened():
        logger.error(f"Failed to open video file: {avi_path}")
        return

    # Frame extraction settings
    frame_interval = config['segmentation'].get('frame_subsample_interval', 10)  # Extract every N-th frame
    frame_count = 0
    saved_count = 0

    while video.isOpened():
        ret, frame = video.read()
        if not ret or frame is None:
            break

        if frame_count % frame_interval == 0:
            saved_count += 1
            out_filename = f"{filename}-TEST-frame_{frame_count:06d}.jpg"
            cv2.imwrite(os.path.join(scratch_path, out_filename), frame)

        frame_count += 1

    video.release()
    logger.info(f"Extracted {saved_count} test frames from {filename} (Interval: Every {frame_interval} frames).")

    # Archive to ZIP and clean scratch
    shutil.make_archive(os.path.join(segmentation_dir, f"{filename}-TEST_EXTRACT"), 'zip', scratch_path)
    delete_file(scratch_path, logger)


if __name__ == "__main__":
    if not os.path.exists('config.json'):
        print("Required configuration file 'config.json' not found. Aborting.")
        sys.exit(1)
    
    with open('config.json', 'r') as f:
        config = json.load(f)

    logger = setup_logger('Frame Extraction (Main)', config)

    # 1. Modify labeling to isolate test outputs from standard segmentation
    base_name = config['segmentation'].get('basename', 'REG')
    config['segmentation']['basename'] = f"{base_name}-TEST_EXTRACT"

    # 2. Directory setup
    directory = sys.argv[-1] if len(sys.argv) > 1 else config.get('raw_dir', './')
    config['raw_dir'] = os.path.abspath(directory)
    config['segmentation_dir'] = constructSegmentationDir(config['raw_dir'], config)

    avis = findVideos(config, logger)
    if not avis:
        logger.warn(f"No video files found in {config['raw_dir']}.")
        sys.exit(0)

    # 3. Apply local system safety limits
    cpu_cap = int(os.cpu_count() * 0.8)
    max_nfs_workers = 4
    num_workers = min(cpu_cap, max_nfs_workers, max(1, len(avis)))

    logger.info(f"Starting test frame extraction across {len(avis)} videos using {num_workers} workers.")

    # 4. Multiprocess execution
    timer_start = time()
    with concurrent.futures.ProcessPoolExecutor(max_workers=num_workers) as executor:
        future_to_file = {
            executor.submit(extract_video_frames, avi, config['segmentation_dir'], config): avi 
            for avi in avis
        }

        for future in concurrent.futures.as_completed(future_to_file):
            avi = future_to_file[future]
            try:
                future.result()
            except Exception as exc:
                logger.error(f"Extraction for {avi} generated an exception: {exc}")

    logger.info(f"Frame extraction complete. Total time: {(time() - timer_start)/60:.1f} minutes.")

    # 5. User scratch cleanup
    logger.info("Running scratch directory cleanup...")
    scratch_dir = config['segmentation'].get('scratch_dir', '/dev/shm/tchen/')
    current_user = getpass.getuser()
    cleanup_command = ['find', scratch_dir, '-user', current_user, '-type', 'f', '-delete']

    try:
        subprocess.run(cleanup_command, check=True)
        logger.info(f"Successfully cleaned scratch directory {scratch_dir} for {current_user}.")
    except Exception as e:
        logger.error(f"Failed scratch cleanup: {e}")

    sys.exit(0)