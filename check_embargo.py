import sys
import os
import logging
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")

def check_embargo_timestamp(file_path: str, output_flag_path: str):
    if not os.path.exists(file_path):
        logging.info(f"Embargo file {file_path} does not exist. Skipping.")
        with open(output_flag_path, "w") as f:
            f.write("false")
        return

    with open(file_path, "r") as f:
        embargo_datetime_str = f.readline().strip()
        logging.info(f"Read string {embargo_datetime_str} from file {file_path}")

    try:
        embargo_datetime = datetime.fromisoformat(embargo_datetime_str)
    except ValueError as e:
        logging.info(f"Invalid timestamp format: {e}. Skipping.")
        with open(output_flag_path, "w") as f:
            f.write("false")
        return

    if embargo_datetime.tzinfo is None:
        embargo_datetime = embargo_datetime.replace(tzinfo=ZoneInfo("Europe/Zurich"))

    now_in_ch = datetime.now(timezone.utc).astimezone(ZoneInfo("Europe/Zurich"))
    delta = now_in_ch - embargo_datetime

    if delta > timedelta(hours=24) or delta < timedelta(seconds=0):
        logging.info("Embargo not valid (too old or in future).")
        with open(output_flag_path, "w") as f:
            f.write("false")
    else:
        logging.info("Embargo is valid.")
        with open(output_flag_path, "w") as f:
            f.write("true")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python check_embargo.py <path_to_embargo_file> <path_to_output_flag>")
        sys.exit(1)

    check_embargo_timestamp(sys.argv[1], sys.argv[2])
