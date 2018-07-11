#!/usr/bin/env bash

hadoop fs -rm measurements.txt
hadoop fs -rm sensor-location.txt

hadoop fs -rm -r measurements-vs-sensor-location
hadoop fs -rm -r item1.out
hadoop fs -rm -r item2.out
hadoop fs -rm -r item3.out