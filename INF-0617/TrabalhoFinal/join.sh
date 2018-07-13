#!/usr/bin/env bash

hadoop jar /home/bitnami/stack/hadoop/share/hadoop/tools/lib/hadoop-streaming-3.1.0.jar \
    -files join-mapper.py,join-reducer.py \
    -mapper join-mapper.py \
    -reducer join-reducer.py \
    -input measurements.txt \
    -input sensor-location.txt \
    -output measurements-vs-sensor-location