import sys
import os
from datetime import datetime, timedelta
from pytz import timezone

def check_embargo_timestamp(file_path: str):
    tz = timezone("Europe/Zurich")
    now = datetime.now(tz)

    if not os.path.exists(file_path):
        print(f"SKIP: Embargo file {file_path} does not exist.")
        sys.exit(0)  # Exit 0 so Airflow marks the task as 'skipped' if you catch this

    with open(file_path, "r") as f:
        timestamp_str = f.read().strip()

    try:
        embargo_time = datetime.fromisoformat(timestamp_str).astimezone(tz)
    except ValueError as e:
        print(f"SKIP: Invalid timestamp format in {file_path}: {e}")
        sys.exit(0)

    if now - embargo_time > timedelta(hours=24):
        print(f"SKIP: Embargo timestamp {embargo_time.isoformat()} is older than 24 hours.")
        sys.exit(0)

    print(f"OK: Embargo timestamp {embargo_time.isoformat()} is recent enough.")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python check_embargo.py <path_to_embargo_file>")
        sys.exit(1)

    file_path = sys.argv[1]
    check_embargo_timestamp(file_path)
