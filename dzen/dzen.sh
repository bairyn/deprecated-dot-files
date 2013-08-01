#!/bin/zsh
FN=`readlink -f $0`
if [ "$FN" != "" ]; then
	cd `dirname $FN`
fi

./date.sh&
./temperature-meter.sh &>> /dev/null&
./battery-meter.sh &>> /dev/null&
if [ $HOST = "OC" ]; then
	./network-meter.sh &>> /dev/null&
else
	./wireless-meter.sh &>> /dev/null&
fi
./memory-meter.sh &>> /dev/null&
./disk-monitor-controls.sh &>> /dev/null&
