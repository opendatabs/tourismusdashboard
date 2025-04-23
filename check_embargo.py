import sys
import os
import logging
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")

SKIP_CODE = 42

def check_embargo_timestamp(file_path: str):
    if not os.path.exists(file_path):
        logging.info(f"Embargo file {file_path} does not exist. Skipping.")
        sys.exit(SKIP_CODE)

    with open(file_path, "r") as f:
        embargo_datetime_str = f.readline().strip()
        logging.info(f"Read string {embargo_datetime_str} from file {file_path}")

    try:
        embargo_datetime = datetime.fromisoformat(embargo_datetime_str)
    except ValueError as e:
        logging.info(f"Invalid timestamp format: {e}. Skipping.")
        sys.exit(SKIP_CODE)

    if embargo_datetime.tzinfo is None:
        logging.info("Datetime string is naive. Adding timezone Europe/Zurich.")
        embargo_datetime = embargo_datetime.replace(tzinfo=ZoneInfo("Europe/Zurich"))

    now_in_ch = datetime.now(timezone.utc).astimezone(ZoneInfo("Europe/Zurich"))
    delta = now_in_ch - embargo_datetime

    logging.info(f"Delta is {delta}")

    if delta > timedelta(hours=24):
        logging.info(f"Embargo timestamp is older than 24 hours. Skipping.")
        sys.exit(SKIP_CODE)

    if delta < 0:
        logging.info(f"Embargo timestamp is in future. Skipping.")
        sys.exit(SKIP_CODE)

    logging.info("Embargo timestamp is within the last 24 hours. Proceeding.")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python check_embargo.py <path_to_embargo_file>")
        sys.exit(1)

    check_embargo_timestamp(sys.argv[1])
