module Config.Queries where

-- import           Config.Core
-- import           Config.Workspaces
--
-- import           Control.Monad
-- import           Data.List
-- import           Data.Maybe
import Data.Monoid

import XMonad (ask, get, gets)
import XMonad.Core
import XMonad.ManageHook
import XMonad.Operations
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.Modal

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.WorkspaceHistory

import Graphics.X11.Xlib.Extras
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

shiftSilent :: WorkspaceId -> ManageHook
shiftSilent tows = do
    fromws <- liftX $ W.currentTag . windowset <$> get
    wid <- ask -- get opened windowId
    doF $ W.view fromws . W.insertUp wid . W.view tows

gotoMain = windows $ W.greedyView "Main"

-- fws <- getFirstWS
-- windows $ W.view fws
-- windows $ W.greedyView fws
-- spawnOnOnce "Main" term
-- windows $ W.greedyView "Main"

-- set <- gets windowset
-- when (W.tagMember "Main" set) $ windows $ W.greedyView "Main"

-- spawn "sleep 5; wmctrl -s 0"
-- windows $ W.greedyView "Main"
-- DO.withNthWorkspace W.greedyView 0
-- DW.addWorkspace "Main"
-- windows   $ W.greedyView "Main"
-- DO.moveToGreedy Next (WSIs $ return (\w -> W.tag w == "Main"))
-- DO.moveToGreedy Prev (WSIs $ return (\w -> W.tag w == "Main"))
-- spawnOnOnce "Main"  term
-- windows $ W.greedyView "Main"
-- DO.withNthWorkspace W.greedyView 0
-- windows (W.greedyView "Main")
-- appendWorkspace "irc"
-- spawnOnce $ term ++ " --name weechat -e weechat"
-- appendWorkspace "email"
-- spawnOnce $ term ++ " --name neomutt -e neomutt"
-- appendWorkspace "music"
-- spawnOnce $ term ++ " --name spt     -e spt"
-- withNthWorkspace W.greedyView 1
-- windows $ W.greedyView "Main"

-- xmStartupHook = mempty
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

isMessage :: Query Bool
isMessage =
    className
        =? "message"
        <||> className
        =? "toolbar"
        <||> className
        =? "error"
        <||> className
        =? "download"
        <||> className
        =? "confirm"
        <||> className
        =? "Toolkit"
        <||> className
        =? "Xmessage"
        <||> className
        =? "Pinentry-gtk2"

-- <||> title     =? "Oracle VM VirtualBox Manager"
-- className =? "mpv"

isGimp :: Query Bool
isGimp =
    className
        =? "Gimp"
        <||> appName
        =? "gimp"

isSpotify :: Query Bool
isSpotify =
    className
        =? "Spotify"
        <||> appName
        =? "spotify"

isFirefox :: Query Bool
isFirefox =
    className
        =? "firefox"
        <||> appName
        =? "Navigator"

-- create some X window rules:
xmManageQueries :: Query (Endo WindowSet)
xmManageQueries =
    composeAll
        [ isGimp --> doFloat
        , isDialog --> doCenterFloat
        , isMessage --> doCenterFloat
        , appName =? "spotify" --> doShift "Music"
        , appName =? "weechat" --> doShift "IRC"
        , appName =? "neomutt" --> doShift "Email"
        , appName =? "main" --> doShift "Main"
        ]

-- xmManageHook :: ManageHook
-- xmManageHook = manageSpawn <> xmManageQueries <> manageHook def
--
-- xmEventHook :: Event -> X All
-- xmEventHook =  swallowEventHook (className =? term) (return True)
--
-- logMode' =
-- modeHook :: X ()
-- modeHook = xmonadPropLog' "_XMONAD_MODE" message
--     where

-- logPP = def { ppOrder = const [], ppExtras = [ logMode ] }

-- xmModeHook = xmonadPropLog' "_XMONAD_LOG_MODE" =<< dynamicLogString logPP

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
-- xmLogHook :: X ()
-- xmLogHook = xmModeHook <> workspaceHistoryHook
