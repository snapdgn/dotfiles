#!/usr/bin/env bash
DIR="$HOME/.config/polybar"
killall -q polybar
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Show on all monitors. Systray icons always go in the main one
monitors=($(xrandr | awk '/ connected / {print $1;}'))
main=0

for ((i=0; i<${#monitors[@]}; i++)); do
    if [ $i -eq $main ]; then
        export TRAY_POS=right
    fi

    MONITOR=${monitors[$i]} polybar -q main -c "$DIR"/config.ini &

    if [ $i -eq $main ]; then
        unset TRAY_POS
    fi
done
polybar -q main -c "$DIR"/config.ini &
