#!/bin/bash

datetime=$(date '+%m-%d-%Y-T-%T')
maim -q -m 10 -i "$(xdotool getactivewindow)" | \
  tee "$HOME/pictures/screenshots/maim-$datetime.png" | \
  xclip -selection clipboard -t image/png && \
  dunstify -a "xmonad" \
    "Screenshot taken" \
    "Screenshot saved to file: maim-$datetime.png"
