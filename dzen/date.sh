#!/bin/zsh
X=0       # x position
Y=1036    # y position
W=150      # width of the dzen bar

zsh -c 'export INTERVAL=1s

while true; do
    date "+%Y-%m-%d_%H:%M:%S"

    sleep $INTERVAL
done
' | dzen2 -x $X -y $Y -w $W -e 'onstart=lower,collapse;onstart=hide;sigusr1=togglehide;sigusr2=unhide' -ta l -fg '#A1A1EF' -bg '#11112F' -fn 'fixed'
