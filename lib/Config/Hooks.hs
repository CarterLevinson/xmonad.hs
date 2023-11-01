module Config.Hooks
    ( xmEventHook
    , xmLogHook
    , xmStartupHook
    , xmManageHook
    ) where

import Config.Static

import Data.Monoid
import Data.Maybe

import XMonad.Core
import XMonad.ManageHook

import XMonad.Hooks.Modal
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory

import XMonad.Util.Run
import Graphics.X11.Xlib.Extras

-- use loginctl? proper order to start these programs?
-- seems to work fine
-- start some nice programs
xmStartupHook :: X ()
xmStartupHook = mempty
-- xmStartupHook = do
    -- appendWorkspace "Spotify"
    -- spawn "spotify-launcher"
    -- appendWorkspace "Gimp"
    -- selectWorkspace "1"
    -- windows $ W.view "1"

-- xmStartupHook = do
--     -- spawnOnce xrandr
--     spawnOnce "~/.fehbg"
--     spawnOnce xscreensaver
--     spawnOnce "dunst"
--     -- appendWorkspace "Spotify"
--     -- spawnOn "Spotify" "spotify-launcer"
--     -- spawnOn windows "Spotify") "spotify-launcher"
--     -- appendWorkspace "Spotify"
--     -- appendWorkspace "Email"
--     -- appendWorkspace "Calendar"
--         where
--             xrandr = "xrandr --output DP1 --primary"
--             xscreensaver = "xscreensaver --no-splash"

shouldCenter :: Query Bool
shouldCenter = isDialog
    <||> className =? "toolbar"
    <||> className =? "error"
    <||> className =? "download"
    <||> className =? "confirm"
    <||> className =? "Toolkit"
    <||> className =? "Xmessage"
    <||> className =? "pinentry"
    -- <||> title     =? "Oracle VM VirtualBox Manager"
    -- className =? "mpv"

isGimp = className =? "Gimp"
    <||> appName =? "gimp"

isSpotify = className =? "Spotify"
    <||> appName =? "spotify"

isFirefox = className =? "firefox"
    <||> appName =? "Navigator"

-- isXmessage = className =? "Xmessage"
--     <||> appName =? "xmessage"
--
-- create some X window rules:
xmManageHook :: Query (Endo WindowSet)
xmManageHook = composeAll
    -- [ isDialog               --> doFloat
    [ shouldCenter            --> doCenterFloat
    , isGimp                 --> doFullFloat
    -- , isXmessage             --> doCenterFloat
    , isSpotify              --> doShift "Spotify"
    ]


xmEventHook :: Event -> X All
xmEventHook =  swallowEventHook (className =? term) (return True)

-- logMode' =
-- modeHook :: X ()
-- modeHook = xmonadPropLog' "_XMONAD_MODE" message
--     where


logPP = def { ppOrder = const [], ppExtras = [ logMode ] }

xmModeHook = xmonadPropLog' "_XMONAD_LOG_MODE" =<< dynamicLogString logPP

-- logMode' = id
--     where
--         logger = logMode . Just
--         message = case logger of
--                     Just s -> ""
--                     Nothing -> ""
--

    -- logMode >>

-- logging: perform an arbitrary action on each internal state change
-- or X event
xmLogHook :: X ()
xmLogHook = xmModeHook >> workspaceHistoryHook
