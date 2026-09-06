import os
import cv2

def is_avi_file(filename):
    return filename.lower().endswith(".avi")

def validate_avi_files(root_dir):
    total = 0
    failed = 0

    for dirpath, _, filenames in os.walk(root_dir):
        for fname in filenames:
            if is_avi_file(fname):
                total += 1
                path = os.path.join(dirpath, fname)
                try:
                    cap = cv2.VideoCapture(path)
                    if not cap.isOpened():
                        raise Exception("Cannot open video file")
                    
                    ret, _ = cap.read()
                    if not ret:
                        raise Exception("Cannot read first frame")
                    
                    cap.release()

                except Exception as e:
                    failed += 1
                    print(f"❌ Corrupt: {path} - {e}")

    print(f"\n✅ Checked {total} AVI files")
    print(f"❌ Corrupt or unreadable: {failed}")

if __name__ == "__main__":
    import sys
    if len(sys.argv) != 2:
        print("Usage: python validate_avi.py /path/to/folder")
    else:
        validate_avi_files(sys.argv[1])
