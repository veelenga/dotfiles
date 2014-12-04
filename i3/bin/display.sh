#!/bin/bash

IN="LVDS1"
EXT=${2:-"VGA1"}

[[ $# -lt 1 ]] && exit 1

case $1 in
  left)
    xrandr --output $IN --auto --output $EXT --auto --primary --left-of $IN
    ;;
  right)
    xrandr --output $IN --auto --output $EXT --auto --primary --right-of $IN
    ;;
  clone)
    xrandr --output $IN --auto --primary --output $EXT --same-as $IN --auto
    ;;
  only)
    if [[ "$IN" == $2 ]]
    then
        xrandr --output $IN --auto --primary --output "VGA1" --off
    else
        xrandr --output $IN --off --output $EXT --auto --primary
    fi
esac

feh --bg-fill ~/.wallpaper
