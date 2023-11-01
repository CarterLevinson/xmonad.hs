#!/bin/bash

ERR=$(xmonad --restart)

if [[ "$ERR" -eq 0 ]]; then
  dunstify -a "xmonad" \
    "XMonad Restarted" \
    "Xmonad has now restarted successfully"
else
  dunstify -a "xmonad" \
    "XMonad Error" \
    "XMonad restart has failed with error code: $ERR"
fi
