#!/usr/bin/env python

import sys

cur_min_temp = 999
cur_max_temp = -999
cur_city = None


def print_out(city_param, min_param, max_param):
    print("('%s', %s, %s)" % (city_param, min_param, max_param))


for line in sys.stdin:
    city, temperature = line.strip().split("\t")
    temperature = int(temperature)

    if cur_city is not None and cur_city != city:
        print_out(cur_city, cur_min_temp, cur_max_temp)
        cur_min_temp = 999
        cur_max_temp = -999

    if temperature > cur_max_temp:
        cur_max_temp = temperature

    if temperature < cur_min_temp:
        cur_min_temp = temperature

    cur_city = city

# Print do ultimo registro
if cur_city is not None:
    print_out(cur_city, cur_min_temp, cur_max_temp)
