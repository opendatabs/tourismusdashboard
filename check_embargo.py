import sys
import os
import logging
from datetime import datetime, timedelta, timezone
from zoneinfo import ZoneInfo

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")

def check_embargo_timestamp(file_path: str):
    if not os.path.exists(file_path):
        logging.info("SKIP: File not found")
        return False

    with open(file_path, "r") as f:
        embargo_datetime_str = f.readline().strip()

    try:
        embargo_datetime = datetime.fromisoformat(embargo_datetime_str)
        if embargo_datetime.tzinfo is None:
            embargo_datetime = embargo_datetime.replace(tzinfo=ZoneInfo("Europe/Zurich"))
    except Exception:
        logging.info("SKIP: Invalid format")
        return False

    now = datetime.now(timezone.utc).astimezone(ZoneInfo("Europe/Zurich"))
    delta = now - embargo_datetime

    if delta > timedelta(hours=24) or delta < timedelta(0):
        logging.info("SKIP: Out of embargo window")
        return False

    logging.info("PROCEED: Within embargo window")
    return True

if __name__ == "__main__":
    result = check_embargo_timestamp(sys.argv[1])
    if result:
        sys.exit(0)
    else:
        sys.exit(99)
