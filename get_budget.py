import imaplib
import socket
import os
import email
import re
import sqlite3
from pathlib import Path
from datetime import datetime
import tomllib
import subprocess
import argparse
import sys

with open("config.toml", "rb") as f:
    config = tomllib.load(f)

IMAP_SERVER = "imap.gmail.com"
USER = "mstanley.yo@gmail.com"
PASS = str(os.environ.get("MS_PASS"))
WD = Path(config["working_directory"])

def init_database():
    """Initialize the SQLite database with the budget_entries table."""
    db_path = WD / "budget.db"
    with sqlite3.connect(db_path) as conn:
        conn.execute("""
            CREATE TABLE IF NOT EXISTS budget_entries (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                entry_datetime TEXT NOT NULL,
                store_name TEXT NOT NULL,
                amount INTEGER NOT NULL,
                logged_at TEXT NOT NULL,
                UNIQUE(entry_datetime, store_name, amount)
            )
        """)
    return db_path

def load_last_run():
    """Load the last run timestamp from last_run.log."""
    logfile = WD / "last_run.log"
    try:
        with open(logfile, "r") as f:
            timestamp_str = f.read().strip()
            return datetime.strptime(timestamp_str, "%Y-%m-%dT%H:%M:%S")
    except FileNotFoundError:
        # Default to a date far in the past if file doesn't exist
        return datetime(2000, 1, 1)

def save_last_run():
    """Save the current timestamp to last_run.log."""
    logfile = WD / "last_run.log"
    with open(logfile, "w") as f:
        f.write(datetime.now().isoformat(timespec="seconds"))

def insert_budget_entry(db_path, entry_dt, store_name, amount):
    """Insert a budget entry into the database."""
    with sqlite3.connect(db_path) as conn:
        try:
            conn.execute(
                """
                INSERT INTO budget_entries (entry_datetime, store_name, amount, logged_at)
                VALUES (?, ?, ?, ?)
                """, (
                    entry_dt.isoformat(timespec="seconds"),
                    store_name,
                    amount,
                    datetime.now().isoformat(timespec="seconds")
                )
            )
            return True
        except sqlite3.IntegrityError:
            # Entry already exists
            return False

def manual_add_entry(entry_string):
    """
    Manually add a budget entry from command line.
    
    Args:
        entry_string: String in format "AMOUNT STORE_NAME" (e.g., "2000 TEST")
    """
    db_path = init_database()
    
    # Parse the entry string
    parts = entry_string.split(None, 1)  # Split on first whitespace
    if len(parts) != 2:
        print("❌ Error: Entry must be in format 'AMOUNT STORE_NAME'")
        print("   Example: python script.py -a '2000 TEST'")
        return
    
    try:
        amount = int(parts[0])
        store_name = parts[1].strip()
    except ValueError:
        print("❌ Error: Amount must be a valid integer")
        return
    
    # Use current datetime for both entry and log time
    current_dt = datetime.now()
    
    # Insert the entry
    if insert_budget_entry(db_path, current_dt, store_name, amount):
        print(f"✅ Manually logged: {current_dt} | {store_name} | ¥{amount:,}")
        
        # Export to CSV
        export_to_csv(db_path)
        print("📄 CSV updated")
    else:
        print(f"⏭️  Duplicate entry: {current_dt} | {store_name} | ¥{amount:,}")

def get_text_body(message):
    """Return the first text/plain body decoded as detected charset."""
    for part in message.walk():
        if part.get_content_type() == "text/plain":
            charset = part.get_content_charset()
            return part.get_payload(decode=True).decode(charset, errors="replace")
    return None

def fetch_emails_since(last_run):
    """
    Connect to IMAP server and fetch emails from Yucho bank since last_run date.
    
    Returns:
        list: List of email message objects, or empty list if error/no messages
    """
    last_run_date = last_run.strftime('%d-%b-%Y')
    
    try:
        print(f"🌐 Trying to connect to {IMAP_SERVER}")
        imap = imaplib.IMAP4_SSL(IMAP_SERVER)
        imap.login(USER, PASS)
        imap.select()
        
        _, msgnums = imap.search(
            None, 
            "FROM", "yuchodebit@jp-bank.japanpost.jp",
            "SINCE", last_run_date
        )
        
        msgnums = msgnums[0].split()
        if not msgnums:
            print("📝 No new emails found.")
            imap.close()
            return []
        
        # parse bytes into just the messages from the email
        messages = []
        for msgnum in msgnums:
            _, data = imap.fetch(msgnum, "(RFC822)")
            if data is not None and len(data) > 0 and data[0] is not None:
                email_data = data[0]
                if isinstance(email_data, tuple) and len(email_data) > 1:
                    raw_email = email_data[1]
                    if isinstance(raw_email, bytes):
                        message = email.message_from_bytes(raw_email)
                        messages.append(message)
        imap.close()
        return messages
        
    except (imaplib.IMAP4.error, imaplib.IMAP4.abort) as e:
        print(f"❌ IMAP error: {e}. Exiting.")
        return []
    except (socket.gaierror, socket.timeout, ConnectionError) as e:
        print(f"❌ Network error: {e}. Exiting.")
        return []
    except Exception as e:
        print(f"❌ Unexpected error: {e}. Exiting.")
        return []
 
def parse_budget_entry(body):
    """
    Parse budget entry information from email body.
    
    Returns:
        tuple: (entry_datetime, store_name, amount) or None if no match
    """
    budget_pattern = re.compile(
        r"利用日時\s+(.+)\n"
        r"利用店舗\s+(.+)\n"
        r"利用金額\s+(.+)円"
    )
    match = budget_pattern.search(body)
    
    if not match:
        return None
    
    entry_dt = datetime.strptime(match.group(1), "%Y/%m/%d %H:%M:%S\r")
    store_name = match.group(2)
    amount = int(match.group(3).replace(",", ""))
    
    return (entry_dt, store_name, amount)

def export_to_csv(db_path):
    """Export all budget entries to a CSV file."""
    import csv
    
    csv_path = WD / "budget_entries.csv"
    
    with sqlite3.connect(db_path) as conn:
        cursor = conn.cursor()
        cursor.execute("""
            SELECT id, entry_datetime, store_name, amount, logged_at
            FROM budget_entries
            ORDER BY entry_datetime DESC
        """)
        
        rows = cursor.fetchall()
        
        with open(csv_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerow(["id", "entry_datetime", "store_name", "amount", "logged_at"])
            writer.writerows(rows)
    
    print(f"📄 Exported {len(rows)} entries to {csv_path}")
    return csv_path

def process_messages(messages, db_path, last_run):
    """
    Process email messages and insert budget entries into the database.
    
    Returns:
        int: Number of new entries added
    """
    new_entries = 0
    
    for message in messages:
        body = get_text_body(message)
        if body is None:
            print("⚠️  Email is not plain text, skipping.")
            continue
        
        parsed = parse_budget_entry(body)
        if parsed is None:
            print("⚠️  No budget pattern match found, skipping.")
            continue
        
        entry_dt, store_name, amount = parsed
        entry_dt, store_name, amount = [
            re.sub(r"\s+", " ", value).strip() 
            if isinstance(value, str) else value
            for value in (entry_dt, store_name, amount)
        ]

        if entry_dt < last_run:
            print(f"⏭️  Entry from {entry_dt} already processed, skipping.")
            continue
        
        # Insert into database
        if insert_budget_entry(db_path, entry_dt, store_name, amount):
            print(f"✅ Logged: {entry_dt} | {store_name} | ¥{amount:,}")
            new_entries += 1
        else:
            print(f"⏭️  Skipping duplicate entry: {entry_dt} | {store_name}")
    
    return new_entries

def run_app():
    try:
        subprocess.run(["Rscript", str(WD / "run_app.R")])
    except KeyboardInterrupt:
        print("Received SIGINT, exiting...")
        sys.exit()

def main():
    parser = argparse.ArgumentParser(description="Budget tracker with email sync and manual entry")
    parser.add_argument(
        "-a", "--add", type=str, metavar="ENTRY",
        help='Manually add entry in format "AMOUNT STORE_NAME" (e.g., "2000 TEST")'
    )
    args = parser.parse_args()

    # Handle manual entry mode
    if args.add:
        manual_add_entry(args.add)
        return

    # Normal operation: fetch emails and process
    # Initialize database
    db_path = init_database()

    # Load last run time
    last_run = load_last_run()
    print("Last run:", last_run)

    # Fetch emails from IMAP server
    try:
        messages = fetch_emails_since(last_run)
        if messages:
            new_entries = process_messages(messages, db_path, last_run)
            print(f"\n📊 Summary: {new_entries} new entries added to database.")
            export_to_csv(db_path)

            # Update last run timestamp
            save_last_run()
            print(f"💾 Updated last_run.log")
    finally:
        print(f"📈 Running shiny app for visualization")
        run_app()

if __name__ == "__main__":
    main()
