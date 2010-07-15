#!/bin/sh

#PBJAR=$QUINTUS/java3.5/prologbeans.jar 
PBJAR=$SICSTUS_HOME/bin/sp-$SICSTUS_VERSION/sicstus-$SICSTUS_VERSION/bin/prologbeans.jar 

java -classpath $PBJAR:./build/classes gov.nih.nlm.nls.metamap.MetaMapApiTest $*


