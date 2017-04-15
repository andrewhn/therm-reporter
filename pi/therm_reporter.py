import requests
import os
import threading
import time

serials = [("reader1", "28-0316554970ff")]
target = "http://10.1.1.4:5353/report"
site_name = "site1"

root = "/sys/bus/w1/devices/{serial}/w1_slave"

## may need to run
# sudo modprobe w1-gpio
# sudo modprobe w1-therm

def get_temp(serial):
    location = root.format(serial=serial)
    with open(location) as f:
        bus = f.readlines()
    temp = float(bus[1].split(" ")[9][2:]) / 1000
    return temp

class ThermalReporter(threading.Thread):

    def __init__(self, serial, name):
        self._serial = serial
        self._name = name
        super(ThermalReporter, self).__init__()

    def run(self):
        deg = get_temp(self._serial)
        requests.post(target, json={"s": site_name, "n": self._name, "t": deg})

while True:
    for name, serial in serials:
        ThermalReporter(serial, name).start()
    time.sleep(1)
