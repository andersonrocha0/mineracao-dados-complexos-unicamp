#!/usr/bin/env python

import sys
import statistics

# Criando um array de temperaturas para representar os 12 meses
temperatures = [[]]*12
cur_city = None


def print_out(city_param, temperatures_param):
    jan_mean = statistics.mean(temperatures_param[0])
    jan_stdev = statistics.stdev(temperatures_param[0])

    jul_mean = statistics.mean(temperatures_param[6])
    jul_stdev = statistics.stdev(temperatures_param[6])
    print("('%s', '%s', '%s', '%s', '%s')" % (city_param, jan_mean, jan_stdev, jul_mean, jul_stdev))


for line in sys.stdin:
    city, month, temperature = line.strip().split("\t")
    temperature = int(temperature)

    if cur_city is not None and cur_city != city:
        print_out(cur_city, temperatures)
        temperatures = [[]]*12

    temperatures[int(month) - 1].append(temperature)

    cur_city = city

# Print do ultimo registro
if cur_city is not None:
    print_out(cur_city, temperatures)
