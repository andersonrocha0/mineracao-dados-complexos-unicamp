#!/usr/bin/env python

import sys
import string



# Store all lines in an array to order before join
lines = []

for line in sys.stdin:
    line = line.strip()
    sensor_id, city, year, month, day, hour, minute, temperature, humidity, lux = line.split("\t")

    values = (sensor_id, city, year, month, day, hour, minute, temperature, humidity, lux)
    lines.append(values)

lines.sort(key=lambda tup: (tup[0], tup[1]))

lines = list(reversed(lines))

# Essa seria uma saida sem a inversao
# 00001	-	2012	07	22	00	19	370	004	503
# 00001	-	2015	12	20	18	01	479	034	160
# 00001	Jan Campinas, SP	-	-	-	-	-	-	-	-

# Com a inversao, a saida sera a seguinte
# 00001	Jan Campinas, SP	-	-	-	-	-	-	-	-
# 00001	-	2015	12	20	18	01	479	034	160
# 00001	-	2012	07	22	00	19	370	004	503

# Desta forma o join sera mais facil de fazer

last_sensor_id = None
cur_city = "-"

for line in lines:

    sensor_id, city, year, month, day, hour, minute, temperature, humidity, lux = line

    if not last_sensor_id or last_sensor_id != sensor_id:
        last_sensor_id = sensor_id
        cur_city = city
    elif sensor_id == last_sensor_id:
        print('%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s' % (sensor_id, cur_city, year, month, day, hour, minute, temperature, humidity, lux))