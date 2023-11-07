#!/bin/bash

# force recompile
touch "$HOME/.xmonad/Main.hs"

# test for display and notify user
if [[ -n "$DISPLAY" ]]; then
  dunstify -a "xmonad" \
    "Recompiling XMonad" \
    "Starting recompilation and reinstallation of XMonad."
fi

xmonad --recompile

if [[ "$?" -eq 0 ]]; then
    dunstify -a "xmonad" \
      "Recompiling XMonad" \
      "XMonad recompilation successful. Installed to '~/bin'."
    touch "$(realpath $(which xmonad))"
  exit 0
else
  dunstify -a "xmonad" -u "CRITICAL" \
    "Recompiling XMonad" \
    "XMonad recompilation failed with an error code."
  exit 1
fi
