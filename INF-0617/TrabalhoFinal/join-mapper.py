#!/usr/bin/env python

import sys

# Characters 1-5: sensor id; 0:5
# Characters 6-9: year (4 digits); 5:9
# Characters 10-11: month (2 digits: 01 - 12); 9:11
# Characters 12-13: day (2 digits); 11:13
# Characters 14-15: hour (2 digits); 13:15
# Characters 16-17: minute (2 digits); 15:17
# Characters 18-20: temperature (3 digits). Celsius x 10; 17:20
# Characters 21-23: humidity (3 digits); 20:23
# Characters 24-26: lux (3 digits). 23:26


for line in sys.stdin:

    sensor_id = "-"
    city = "-"
    year = "-"
    month = "-"
    day = "-"
    hour = "-"
    minute = "-"
    temperature = "-"
    humidity = "-"
    lux = "-"

    
    key_value = line.strip().split("\t")

    if (len(key_value) == 1):
        # If key value length is 1 the file is measurements.txt
        measurements = key_value[0]
        
        sensor_id = measurements[0:5]
        year = measurements[5:9]
        month = measurements[9:11]
        day = measurements[11:13]
        hour = measurements[13:15]
        minute = measurements[15:17]
        temperature = measurements[17:20]
        humidity = measurements[20:23]
        lux = measurements[23:26]

    else:
        # If key value length is 2 the file is sensor-location.txt
        sensor_id = key_value[0].zfill(5)
        city = key_value[1]

    
    print('%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s' % (sensor_id, city, year, month, day, hour, minute, temperature, humidity, lux))
