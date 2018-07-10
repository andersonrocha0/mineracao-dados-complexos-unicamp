hadoop jar /home/bitnami/stack/hadoop/share/hadoop/tools/lib/hadoop-streaming-3.1.0.jar \
    -D stream.num.map.output.key.fields=2 \
    -D mapred.output.key.comparator.class=org.apache.hadoop.mapred.lib.KeyFieldBasedComparator \
    -D mapred.text.key.comparator.options=-r \
    -files join-mapper.py,join-reducer.py \
    -mapper join-mapper.py \
    -reducer join-reducer.py \
    -input measurements-with-500-lines.txt \
    -input sensor-location.txt \
    -output join-measurements-sensor-location