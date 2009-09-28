#!/bin/sh
#
# Build index for Semantic Type Indexing Method (wsd.methods.SemTypeIndexingMethod)
# example usage:
#    % bin/create_sti_index.sh index wsdvsc01-12-dc.txt wstv-dc
#

if [ $# = 0 ]; then
    echo "usage: $0 [index <sourcefile> <indexdir>] [get <indexdir> <term>]"
    exit 0
fi 

LIB=$WSD/WSD_Server/lib
JAR_FILE=$LIB/wsd.jar

$JAVA_HOME/bin/java -mx2g -classpath $JAR_FILE wsd.util.IntArrayIndex $*

exit 1
