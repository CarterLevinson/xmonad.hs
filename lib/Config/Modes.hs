module Config.Modes where

import Config.Core
import Config.Utils

import Graphics.X11
import XMonad.Core

import Data.Map as M (fromList, empty)

import System.Exit

import XMonad.Hooks.Modal

xmModes :: [Mode]
xmModes =
    [ mode "exit" $ \_ -> M.fromList exitKeys
    , mode "resize" $ const M.empty
    , mode "prompt" $ const M.empty
    , mode "search" $ const M.empty
    , mode "gaps" $ const M.empty
    ]

exitKeys :: [KeyMap]
exitKeys =
    [ ((noMod, xK_l), exitMode >> spawn "xset s activate")
    , ((noMod, xK_h), exitMode >> spawn "systemctl hibernate")
    , ((noMod, xK_s), exitMode >> spawn "systemctl suspend")
    , ((noMod, xK_p), exitMode >> spawn "systemctl poweroff")
    , ((noMod, xK_r), exitMode >> spawn "systemctl reboot")
    , ((noMod, xK_x), exitMode >> io exitSuccess)
    , ((noMod, xK_q), exitMode >> scriptify "notify-restart-xmonad.sh")
    , ((noMod, xK_c), exitMode >> scriptify "notify-recompile-xmonad.sh")
    ]
