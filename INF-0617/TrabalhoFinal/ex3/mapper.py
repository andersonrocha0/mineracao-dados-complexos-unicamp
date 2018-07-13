#!/usr/bin/env python

import sys

for line in sys.stdin:
    sensor_id, city, year, month, day, hour, minute, temperature, humidity, lux = line.strip().split("\t")

    month_number = int(month)
    if month_number == 1 or month_number == 7:
        print('%s\t%s\t%s' % (city, month, temperature))
