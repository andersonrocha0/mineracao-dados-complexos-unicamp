#!/usr/bin/env bash

# O parametro <stream.num.map.output.key.fields=2> informa pro hadoop que vamos usar os dois primeiros campos na ordenacao do map
# O parametro <mapred.output.key.comparator.class> indica que vamos usar a classe KeyFieldBasedComparator para ordenar nossa saida no map
# O parametro <mapred.text.key.comparator.options=-r> indica que vamos ordenar de forma reversa (tambem na saida do map), exemplifico abaixo:

# Essa seria uma saida sem a inversao
# 00001	-	2012	07	22	00	19	370	004	503
# 00001	-	2015	12	20	18	01	479	034	160
# 00001	Jan Campinas, SP	-	-	-	-	-	-	-	-

# Com a inversao, a saida sera a seguinte
# 00001	Jan Campinas, SP	-	-	-	-	-	-	-	-
# 00001	-	2015	12	20	18	01	479	034	160
# 00001	-	2012	07	22	00	19	370	004	503

# Dessa forma, o nosso reducer consegue fazer o join sem muito esforco

hadoop jar /home/bitnami/stack/hadoop/share/hadoop/tools/lib/hadoop-streaming-3.1.0.jar \
    -D stream.num.map.output.key.fields=2 \
    -D mapred.output.key.comparator.class=org.apache.hadoop.mapred.lib.KeyFieldBasedComparator \
    -D mapred.text.key.comparator.options=-r \
    -files join-mapper.py,join-reducer.py \
    -mapper join-mapper.py \
    -reducer join-reducer.py \
    -input measurements.txt \
    -input sensor-location.txt \
    -output measurements-vs-sensor-location