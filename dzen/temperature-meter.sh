#!/bin/bash
#
# by lyon8 (lyon8@gmx.net)
# show your laptop battery state in dzen
 
BG='#11112F'  # dzen backgrounad
FG='#A1A1EF'  # dzen foreground
#W=150     # width of the dzen bar
W=50     # width of the dzen bar
#GW=130      # width of the gauge
GW=8
GFG='#999'  # color of the gauge
GH=7       # height of the gauge
GBG='#444444'  # color of gauge background
X=150     # x position
Y=1036    # y position
FN='fixed' # font
 
#STATEFILE='/sys/acpi/thermal_zone/THRM/temperature' # battery's state file
STATEFILE='/sys/devices/virtual/thermal/thermal_zone0/temp' # battery's state file

LOWBAT=60        # percentage of battery life marked as low
LOWCOL='#44EE44' # color when battery is low
TIME_INT=1         # time intervall in seconds

PREBAR='^i(/home/bob/dzen/icons/dzen_bitmaps/cpu.xbm)' # caption (also icons are possible)
 
while true; do
# look up battery's data
BAT_FULL=100
#STATUS=`cat $STATEFILE|cut -d " " -f 14`;
STATUS=`cat $STATEFILE`;
RCAP=$STATUS

# calculate remaining power
RPERCT=`expr $RCAP \* 100`;
RPERC=`expr $RPERCT / $BAT_FULL`;
 
# draw the bar and pipe everything into dzen
if [ $RPERC -ge $LOWBAT ]; then GFG=$LOWCOL; fi
echo -n $PREBAR
eval echo $RPERC | gdbar -h $GH -w $GW -fg $GFG -bg $GBG
sleep $TIME_INT;
done | dzen2 -ta l -tw $W -y $Y -x $X -fg $FG -bg $BG -fn $FN -e 'onstart=lower,collapse;onstart=hide;sigusr1=togglehide;sigusr2=unhide'
