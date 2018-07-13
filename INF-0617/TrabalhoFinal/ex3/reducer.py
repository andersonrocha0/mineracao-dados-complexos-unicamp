#!/usr/bin/env python

import sys
import math

# Criando um array de temperaturas para representar os 12 meses
temperatures = [[] for i in range(12)]
cur_city = None


def mean(values):
    return sum(values) / len(values)


def stdev(values):
    return math.sqrt(sum([(x - mean(values))**2 for x in values])/len(values))


def print_out(city_param, temperatures_param):
    jan_mean = mean(temperatures_param[0]) if len(temperatures_param[0]) > 0 else 0
    jan_stdev = stdev(temperatures_param[0]) if len(temperatures_param[0]) > 0 else 0

    jul_mean = mean(temperatures_param[6]) if len(temperatures_param[6]) > 0 else 0
    jul_stdev = stdev(temperatures_param[6]) if len(temperatures_param[6]) > 0 else 0
    print("('%s', %s, %s, %s, %s)" % (city_param, int(jan_mean), int(jan_stdev), int(jul_mean), int(jul_stdev)))


for line in sys.stdin:
    city, month, temperature = line.strip().split("\t")
    temperature = int(temperature)

    if cur_city is not None and cur_city != city:
        print_out(cur_city, temperatures)
        temperatures = [[] for i in range(12)]

    temperatures[int(month) - 1].append(temperature)

    cur_city = city

# Print do ultimo registro
if cur_city is not None:
    print_out(cur_city, temperatures)
