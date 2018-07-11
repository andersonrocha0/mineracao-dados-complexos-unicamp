#!/usr/bin/env python

import sys
import calendar

cur_temp = -999
month_with_max = None
city_with_max = None


def get_month_name(month_number):
    return calendar.month_name[int(month_number)]


def print_out(month_param, city_param):
    print("('%s', '%s')" % (get_month_name(month_param), city_param))


for line in sys.stdin:
    month, city, temperature = line.strip().split("\t")
    temperature = int(temperature)

    if month_with_max is not None and month_with_max != month:
        print_out(month_with_max, city_with_max)
        cur_temp = -999

    if temperature > cur_temp:
        cur_temp = temperature
        month_with_max = month
        city_with_max = city

# Print do ultimo registro
if month_with_max is not None:
    print_out(month_with_max, city_with_max)
