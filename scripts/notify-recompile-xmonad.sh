#!/bin/bash

# force recompile
touch "$HOME/.xmonad/Main.hs"

# test for display and notify user
if [[ -n "$DISPLAY" ]]; then
  dunstify -a "xmonad" \
    "Recompiling XMonad" \
    "Starting recompilation and reinstallation of XMonad."
fi

# recompile xmonad
xmonad --recompile
# set the error code
ERR="$?"

# XMonad will launch xmessage with build errors in case of failure
if [[ -n "$DISPLAY" && "$ERR" -eq 0  ]]; then
  dunstify -a "xmonad" \
    "Recompiling XMonad" \
    "XMonad recompilation successful. Installed to '~/bin'"
fi

# exit with same code as xmonad recompile
exit "$ERR"
