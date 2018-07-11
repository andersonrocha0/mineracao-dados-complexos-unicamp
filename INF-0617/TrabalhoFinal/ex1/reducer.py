#!/usr/bin/env python

import sys
import calendar

cur_temp = -999
month_with_max = None
city_with_max = None

def getMonthName(monthNumber):
    return calendar.month_name[int(monthNumber)]

def printOut(month, city):
    print("('%s', '%s')" % (getMonthName(month), city))

for line in sys.stdin:
    month, city, temperature = line.strip().split("\t")
    tempature = int(temperature)

    if month_with_max is not None and month_with_max != month:
        printOut(month_with_max, city_with_max)
        cur_temp = -999

    if temperature > cur_temp:
        cur_temp = temperature
        month_with_max = month
        city_with_max = city

# Print do ultimo registro
if month_with_max is not None:
    printOut(month_with_max, city_with_max)
