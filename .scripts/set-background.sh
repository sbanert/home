#!/bin/sh

BACKGROUNDDIR="/usr/share/backgrounds/archlinux"
BACKGROUNDFILE="archwave.png"
BACKGROUNDPATH="${BACKGROUNDDIR}/${BACKGROUNDFILE}"

if [ -f $BACKGROUNDPATH ]; then
  feh --bg-scale $BACKGROUNDPATH
else
  feh --bg-scale ~/Pictures/wallpapers/Kite.jpg
fi
