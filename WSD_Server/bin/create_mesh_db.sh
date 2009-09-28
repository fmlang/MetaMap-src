#!/bin/sh

UMLSKS_HOST=umlsks
UMLS_RELEASE=2002AD
LIB=$WSD/WSD_Server/lib
JAR_FILE=$LIB/wsd.jar:$LIB/kss-api.jar:$LIB/thirdparty.jar:$LIB/db.jar

java -classpath $JAR_FILE wsd.util.MeSHFrequencyCalculator $UMLSKS_HOST $UMLS_RELEASE $1 $1.txt 
#cat $1.txt | cut -d'|' -f2,3 | sed 's/\\/\\\\/g' | db_load -c duplicates=0 -T -t btree $1.db

