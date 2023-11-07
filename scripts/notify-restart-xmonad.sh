#!/bin/bash

xmonad --restart
ERR="$?"

if [[ "$ERR" -eq 0 ]]; then
  dunstify -a "xmonad" \
    "XMonad Restarted" \
    "Xmonad has now restarted successfully"
  exit 0
else
  dunstify -a "xmonad" -u "CRITICAL" \
    "XMonad Restarted" \
    "XMonad restart has failed with an error code"
  exit 1
fi
