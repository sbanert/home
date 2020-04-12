#!/bin/sh

if [ -f "/usr/share/backgrounds/archlinux/archlinux-arrival.jpg" ]; then
  feh --bg-scale /usr/share/backgrounds/archlinux/archlinux-arrival.jpg
else
  feh --bg-scale ~/Pictures/wallpapers/Kite.jpg
fi
