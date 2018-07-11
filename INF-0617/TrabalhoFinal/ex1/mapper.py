#!/usr/bin/env python

import sys

for line in sys.stdin:
    sensor_id, city, year, month, day, hour, minute, temperature, humidity, lux = line.strip().split("\t")
    print('%s\t%s\t%s' % (month, city, temperature))