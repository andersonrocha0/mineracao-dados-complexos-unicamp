#!/usr/bin/env python

import sys
import string

last_sensor_id = None
cur_city = "-"

for line in sys.stdin:
    line = line.strip()
    sensor_id, city, year, month, day, hour, minute, temperature, humidity, lux = line.split("\t")

    if not last_sensor_id or last_sensor_id != sensor_id:
        last_sensor_id = sensor_id
        cur_city = city
    elif sensor_id == last_sensor_id:
        print('%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s' % (sensor_id, cur_city, year, month, day, hour, minute, temperature, humidity, lux))