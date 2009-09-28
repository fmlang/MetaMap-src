#!/bin/sh
PID_FILE=./pid
if [ -r $PID_FILE ] 
then
	PID=`cat "$PID_FILE"` 
	echo Stopping WSD Server process..
	kill -9 $PID
	echo Process $PID stopped 
        rm $PID_FILE
else
	echo Server not running
	exit 1
fi
