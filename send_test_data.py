import csv
import json
import random
import socket
import time

# Configuration
UUID_FILE_PATH = "resources/uuid-to-device.csv"  # Path to the CSV file
UDP_HOST = "localhost"  # Host of the UDP server
UDP_PORT = 5000         # Port of the UDP server
SLEEP_INTERVAL = 1      # Interval in seconds between sending test data

# Load UUIDs from CSV
def load_uuids(file_path):
    uuids = []
    with open(file_path, newline='') as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            uuid = row[0]
            uuids.append(uuid)
    return uuids

# Generate random test data
def generate_test_data(uuid):
    return {
        "uuid": uuid,
        "rssi_raw": random.randint(-100, -30),  # Random RSSI between -100 and -30
        "rssi_filtered": random.uniform(-100, -30),  # Random filtered RSSI as a float
        "distance_raw": random.uniform(0.1, 10.0),  # Random raw distance in meters
        "distance_filtered": random.uniform(0.1, 10.0),  # Random filtered distance
        "tx_power": random.uniform(-60, -20)  # Random tx_power between -60 and -20
    }

# Send test data to UDP server
def send_test_data(uuids, host, port, interval):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        while True:
            uuid = random.choice(uuids)
            data = generate_test_data(uuid)
            json_data = json.dumps(data)
            sock.sendto(json_data.encode('utf-8'), (host, port))
            print(f"Sent data to {host}:{port} - {json_data}")
            time.sleep(interval)
    except KeyboardInterrupt:
        print("Stopped sending test data.")
    finally:
        sock.close()

# Main function
if __name__ == "__main__":
    uuids = load_uuids(UUID_FILE_PATH)
    if not uuids:
        print("No UUIDs found in CSV file.")
    else:
        print(f"Loaded {len(uuids)} UUIDs from {UUID_FILE_PATH}")
        send_test_data(uuids, UDP_HOST, UDP_PORT, SLEEP_INTERVAL)

