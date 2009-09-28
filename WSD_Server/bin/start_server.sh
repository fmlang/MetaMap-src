#!/bin/sh
JAVA=/nfsvol/cgsb_share/jdk1.4.1/bin/java

if [ ! -n "${WSD}" ]
then
  if [ ! -n "${NLS}" ]
  then
    NLS=/nfsvol/nls
    export NLS
  fi
  WSD=$NLS/tools/Disambiguator
  export WSD
fi

#JAVA=${JAVA_HOME}/bin/java
JAR_FILE=$WSD/WSD_Server/lib/wsd.jar:$WSD/WSD_Server/lib/kss-api.jar:$WSD/WSD_Server/lib/thirdparty.jar:$WSD/WSD_Server/lib/db.jar:$WSD/WSD_Server/lib/log4j-1.2.8.jar
JVMOPTIONS="-Xmx1g -Dserver.config.file=$WSD/WSD_Server/config/disambServer.cfg"
LD_LIBRARY_PATH=$WSD/WSD_Server/lib:/net/pax/usr/local/BerkeleyDB.4.1/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH

if [ $# = 0 ]
then
	rm -rf $WSD/WSD_Server/log/WSD_Server.log*
	$JAVA $JVMOPTIONS -classpath $JAR_FILE wsd.server.DisambiguatorServer  & echo $! > pid
else
	echo Unexpected number of arguments.
	echo Usage: start_server.sh
fi

