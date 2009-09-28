#!/bin/sh

if [ $# = 0 ]; then
    echo "usage: $0 srctable dbfilename"
    exit 0
fi

#DC_ST_TABLE=dc-st.gt10.txt
DC_ST_TABLE=wstvsc01-12-dc.txt

LIB=$WSD/WSD_Server/lib
JAR_FILE=$LIB/wsd.jar:$LIB/kss-api.jar:$LIB/thirdparty.jar:$LIB/db.jar

LD_LIBRARY_PATH=$WSD/WSD_Server/lib:/usr/local/BerkeleyDB.4.1/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

java -classpath $JAR_FILE wsd.util.StiPopulateDb $1 $2


exit 1