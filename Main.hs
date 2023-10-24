{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}


module Main (main) where

import           XMonad.Config.Action
import           XMonad.Config.Data
import           XMonad.Config.Layout
import           XMonad.Config.Prompt
import           XMonad.Config.Static
import           XMonad.Config.Util
import           XMonad.Config.Workspace

-- import           Control.Monad
-- import qualified Data.Text                          as T
import qualified Data.Map                           as M
import           Data.Monoid
import           System.Exit

import           XMonad

import qualified XMonad.StackSet                    as W


import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.SpawnOn

import           XMonad.Hooks.DynamicIcons          (appIcon, iconsPP)
import           XMonad.Hooks.EwmhDesktops          (ewmh, ewmhFullscreen)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers         (isDialog)
import           XMonad.Hooks.Modal
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.Script
import           XMonad.Hooks.WindowSwallowing      (swallowEventHook)
import           XMonad.Hooks.WorkspaceHistory      (workspaceHistoryHook)


import           XMonad.Layout.BinarySpacePartition hiding (Swap)
import           XMonad.Layout.Hidden               (hideWindow,
                                                     popNewestHiddenWindow)
import           XMonad.Layout.WindowNavigation     (Navigate (..))


import           XMonad.Util.EZConfig               (additionalKeys, removeKeys)
import           XMonad.Util.Paste                  (pasteSelection)
import           XMonad.Util.SpawnOnce              (spawnOnce)
import           XMonad.Util.Ungrab                 (unGrab)
import           XMonad.Util.WorkspaceCompare       (getSortByIndex)

-- import qualified XMonad.Actions.Search              as S
-- import           XMonad.Layout.SubLayouts

-- import           XMonad.Layout.Fullscreen

-- import           XMonad.Actions.TopicSpace          as TS
-- import           XMonad.Util.NamedScratchpad


-- TODO: dmenu
-- TODO: Advanced layuouts config
-- TODO: XMonad.contrib modal
-- TODO: more workspace and monitor control
-- TODO: scratchpad workspace
-- TODO: xmobar pretty print & xmonad log hooks
-- TODO: XMonad layout internals
-- TODO: greedy view and view?


-- confirm :: String -> X () -> X ()
-- confirm m f = do
--     result <- D.dmenu [m]
--     when (init result == m) f

-- use loginctl? proper order to start these programs?
-- seems to work fine
-- start some nice programs
xmStartupHook :: X ()
xmStartupHook = do
    spawnOnce xrandr
    spawnOnce "~/.fehbg"
    spawnOnce xscreensaver
    spawnOnce "dunst"
    -- appendWorkspace "Spotify"
    -- spawnOn "Spotify" "spotify-launcer"
    -- spawnOn windows "Spotify") "spotify-launcher"
    -- appendWorkspace "Spotify"
    -- appendWorkspace "Email"
    -- appendWorkspace "Calendar"
        where
            xrandr = "xrandr --output DP1 --primary"
            xscreensaver = "xscreensaver --no-splash"



-- create some X window rules:
-- look into the hook helpers import
xmManageHook :: Query (Endo WindowSet)
xmManageHook = composeAll
  [ isDialog                                     --> doFloat
  , className =? "mpv"                           --> doFloat
  , className =? "Gimp"                          --> doFloat
  , className =? "toolbar"                       --> doFloat
  , className =? "confirm"                       --> doFloat
  , className =? "error"                         --> doFloat
  , className =? "download"                      --> doFloat
  , className =? "notification"                  --> doFloat
  , className =? "Toolkit"                       --> doFloat
  , className =? "Xmessage"                      --> doFloat
  , className =? "pinentry"                      --> doFloat
  , className =? "pinentry-qt"                   --> doFloat
  , title     =? "Oracle VM VirtualBox Manager"  --> doFloat
  ]

xmEventHook :: Event -> X All
xmEventHook = swallowEventHook (className =? "kitty") (return True)

-- logging: perform an arbitrary action on each internal state change
-- or X event
xmLogHook :: X ()
xmLogHook = workspaceHistoryHook

------------------------------------------------------------------------------

xmPP :: PP
xmPP = def
    { ppCurrent = xmobarColor "white" "" . wrap "[" "]"
    , ppVisible = wrap "<" ">"
    , ppUrgent  = xmobarColor "red"   "" . wrap "!" "!"
    -- , ppTitle   = shorten 80
    , ppTitle   = id
    , ppLayout  = last . words
    , ppExtras  = []
    , ppSort    = getSortByIndex
    -- , ppHidden  = id
    -- , ppHiddenNoWindows = const ""
    -- , ppVisibleNoWindows = Nothing
    -- , ppPrinters = empty
    }



xmIconsPP :: X PP
xmIconsPP = iconsPP icons xmPP
    where
        icons = composeAll
            [ className =? "Firefox"  <||> className =? "firefox"   --> appIcon "\xE745"
            , className =? "Chromium" <||> className =? "chromium"  --> appIcon "\xE743"
            , className =? "Spotify"  <||> className =? "spotify"   --> appIcon "\xF1BC"
            , className =? "kitty"    <||> className =? "alacritty" --> appIcon "\xE795"
            ]


xmobar :: StatusBarConfig
xmobar = statusBarPropTo "_XMONAD_LOG_0" "xmobar" (pure xmPP)


------------------------------------------------------------------------------

notify :: String -> String -> X ()
notify msg body = spawn ("notify-send " ++ enquote msg ++ " " ++ enquote body)
    where
        enquote str = wrap str "'" "'"

restartXmonad :: X ()
restartXmonad = do
    notify "Restarting XMonad" "NOW"
    spawn "xmonad --restart"

recompileXmonad :: X ()
recompileXmonad = do
    notify "Compilation Started" "NOW"
    runInShell "xmonad --recompile"
    notify "Compilation Completed" "NOW"

reloadXmonad :: X ()
reloadXmonad = do
    recompileXmonad
    restartXmonad

dmenuRun :: X ()
dmenuRun = spawn "dmenu_run -b -h 20 -p 'Yes, Master?'"


screenshotCopy :: ScreenshotAction -> X()
screenshotCopy ssa = spawn (maim ++ " | tee " ++ file ++ " | " ++ clip)
    where
        file = "~/pictures/screenshots/maim-$(date '+%m-%d-%Y-T-%T').png"
        clip = "xclip -selection clipboard -t image/png"
        base = "maim -q -m 10"
        maim = case ssa of
                 FullScreen ->
                     base
                 FocusedWindow ->
                     base ++ " -i $(xdotool getactivewindow)"
                 SelectedWindow ->
                     base ++ " -s"


-- cycleMainLayouts :: X ()
-- cycleMainLayouts = cycleThroughLayouts ["BSP", "Spiral", "Mosaic" ]
--
-------------------------------------------------------------------------------

xmWS :: [String]
xmWS = map show ([1 .. 9]::[Integer])


-------------------------------------------------------------------------------



-- xmModePairs =
--
-- -- xmModes = zipWith \(md, km) -> mode md k names maps
--     where (names, keyMaps) = unzip xmModePairs
--         maps = map M.fromList keyMaps
--         modes = map mode names


-- shellExit cmd = doExit $ runInShell cmd

-- makeMode name keyMap = mode name $ \_ M.fromList keyMap

-- xmModes = zipWith makeMode names maps
--     where
--         (names, maps) = unzip
--                 [ ("exit", exitKeys)
--                 , ("prompt", promptKeys)
--                 , ("resize", resizeKeys)
--                 , ("search", searchKeys)
--                 ]



xmModes :: [Mode]
xmModes =
    [ xmExitMode
    , xmResizeMode
    , xmSearchMode
    , xmPromptMode
    ]

-- makeMode =

xmExitMode :: Mode
xmExitMode = mode "exit" $ \_ -> M.fromList
    [ ((noMod, xK_l), spawnExit "xscreensaver-command -lock")
    , ((noMod, xK_p), spawnExit "systemctl poweroff")
    , ((noMod, xK_r), spawnExit "systemctl reboot")
    , ((noMod, xK_s), spawnExit "systemctl suspend")
    , ((noMod, xK_h), spawnExit "systemctl hibernate")
    , ((noMod, xK_q), spawnExit "xmonad --restart")
    , ((noMod, xK_c), shellExit "xmonad --recompile")
    , ((noMod, xK_x), io exitSuccess)
    ]
    where
        doExit m = exitMode >> m
        spawnExit cmd = doExit $ spawn cmd
        shellExit cmd = doExit $ runInShell cmd


-- xmLaunchMode :: Mode
-- xmLaunchMode = mode "launch" $ \_ -> M.fromList lst
--   where
--     lst :: [KeyMap]
--     lst =
--       let runExit   str = runInShell str >> exitMode
--           spawnExit str = spawn str      >> exitMode
--        in [ ((noMod, xK_f), runExit   "ranger")
--           , ((noMod, xK_h), runExit   "htop")
--           , ((noMod, xK_e), runExit   "nvim /tmp/tmp.txt")
--           , ((noMod, xK_c), runExit   "bc -q" )
--           , ((noMod, xK_s), spawnExit "spotify-launcher")
--           , ((noMod, xK_b), spawnExit "qutebrowser")
--           , ((noMod, xK_w), spawnExit "firefox")
--           , ((modm, xK_w),  spawnExit "chromium")
--           ]
--

xmResizeMode :: Mode
xmResizeMode = mode "resize" $ \_ -> M.fromList []

xmPromptMode :: Mode
xmPromptMode = mode "prompt" $ \_ -> M.fromList []

xmSearchMode :: Mode
xmSearchMode = mode "search" $ \_ -> M.fromList []

mapModAndModShift :: [KeySym] -> [KeyPair]
mapModAndModShift kps = map2 kps (modm, ) (modShift, )

main :: IO()
main =
    xmonad
    . ewmh
    . ewmhFullscreen
    . docks
    . modal xmModes
    . withSB xmobar
    $ def
        { terminal = term
        , borderWidth = 2
        , modMask = modm
        , workspaces = xmWS
        , normalBorderColor = purple
        , focusedBorderColor = gray
        , layoutHook = xmLayoutHook
        , logHook = xmLogHook
        , manageHook = xmManageHook <+> manageSpawn <+> manageHook def
        -- , manageHook = xmManageHook <+> manageHook def
        , handleEventHook = xmEventHook
        , startupHook = xmStartupHook
        , focusFollowsMouse = False
        , clickJustFocuses = True
        }
    `removeKeys`
        mapModAndModShift
            [xK_a .. xK_z]
        ++
        mapModAndModShift
            [xK_1 .. xK_9]
        ++
        mapModAndModShift
            [ xK_Return
            , xK_comma
            , xK_period
            , xK_slash
            , xK_space
            ]
    `additionalKeys`
        -- Mod+hjkl: vi-style switch focused window
        [ ((modm, xK_h),             sendMessage $ Go L)
        , ((modm, xK_j),             sendMessage $ Go D)
        , ((modm, xK_k),             sendMessage $ Go U)
        , ((modm, xK_l),             sendMessage $ Go R)

        -- Mod+HJKL: vi-style move focused window
        , ((modShift, xK_h),         sendMessage $ Swap L)
        , ((modShift, xK_j),         sendMessage $ Swap D)
        , ((modShift, xK_k),         sendMessage $ Swap U)
        , ((modShift, xK_l),         sendMessage $ Swap R)

        -- Alt+Tab, Alt+Shift+Tab: cycle through focused windows
        , ((alt, xK_Tab),            windows W.focusUp)
        , ((altShift, xK_Tab),       windows W.focusDown)


        -- Layouts

        -- Mod+space: move to next layout in layout hook
        , ((modm, xK_space),         sendMessage NextLayout)
        -- Mod+Shift+space: jump to first layout in layout hook
        , ((modShift, xK_space),     sendMessage $ JumpToLayout "BSP")
        -- ModShift+t: toggle tabbed layout
        , ((modShift, xK_t),         sendMessage toggleTabs)

        -- ModShift+f: toggle full layout
        , ((modShift, xK_f),         sendMessage toggleFull)

        -- ModShift+m: toggle magnify window
        , ((modShift, xK_m),         sendMessage toggleMagnify)
        -- Toggle gaps

        -- Mod+s: "sinks" floating window back into tiled layout
        , ((modm, xK_s) ,            withFocused $ windows . W.sink)

        -- Mod+t: toggles status bar struts
        , ((modm, xK_t),             sendMessage ToggleStruts)

        -- Mod+Plus, Mod+Minus: increase/decrease magnification zoom commands
        , ((modm, xK_plus),          sendMessage increaseMagnify )
        , ((modm, xK_minus),         sendMessage decreaseMagnify)

        -- Mod+backslash, ModShift+backslash: hide / restore hidden windows
        , ((modm, xK_backslash),     withFocused hideWindow)
        , ((modShift, xK_backslash), popNewestHiddenWindow)

        -- Workspaces

        -- Mod+Tab, Mod+Shift+Tab: cycle through populated workspaces
        , ((modm, xK_Tab),           moveTo Next (Not emptyWS))
        , ((modShift, xK_Tab),       moveTo Prev (Not emptyWS))

        -- Cycle Workspaces and Screens

        -- Mod+Up, Mod+Down: cycle through all available workspaces
        , ((modm, xK_Up),            nextWS)
        , ((modm, xK_Down),          prevWS)
        -- Mod+Right, Mod+Left: cycle through all attached screens
        , ((modm, xK_Right),         nextScreen)
        , ((modm, xK_Left),          prevScreen)

        -- ModShift+Up, ModShift+Down: shift to next/prev workspace
        , ((modShift, xK_Up),        shiftToNext)
        , ((modShift, xK_Down),      shiftToPrev)
        -- Modshift+Right, ModShift+Left: shift to next/prev screen
        , ((modShift, xK_Right),     shiftNextScreen)
        , ((modShift, xK_Left),      shiftPrevScreen)

        -- ModCtrl+Up, ModCtrl+Down: shift and move to next/prev workspace
        , ((modCtrl, xK_Up),         shiftToNext >> nextWS)
        , ((modCtrl, xK_Down),       shiftToPrev >> prevWS)
        -- ModCtrl+Right, ModCtrl+Left: shift and move to next/prev screen
        , ((modCtrl, xK_Right),      shiftNextScreen >> nextScreen)
        , ((modCtrl, xK_Left),       shiftPrevScreen >> prevScreen)

        -- Dynamic Workspaces

        -- Mod+a: create and append new named workspace using prompts
        , ((modm, xK_a),             appendWorkspace')
        -- ModShift+a:

        -- Mod+r: rename current workspace using prompts
        , ((modm, xK_r),             renameWorkspace')

        -- Mod+w: select or create workspace using prompts
        , ((modm, xK_w),             selectWorkspace')

        -- ModShift+w: move focused window to new workspace using prompts
        , ((modShift, xK_w),         shiftWorkspace')

        -- ModCtrl+w: copy focused window to new workspace using prompts
        , ((modCtrl, xK_w),          copyWorkspace')

        -- ModShift+BackSpace, delete the current workspace
        , ((modShift, xK_BackSpace), removeWorkspace)

        -- Program Launchers

        , ((modm, xK_q),             kill)

        , ((modm, xK_d),             dmenuRun)

        , ((modm, xK_c),             gsLaunchDotfiles)
        , ((modm, xK_m),             gsLaunchBookmarks)
        , ((modm, xK_o),             gsLaunchApps)
        , ((modm, xK_z),             zshShellPrompt)
        , ((modm, xK_x),             xmonadCmdPrompt)


        , ((modm,     xK_Return),    runInShell "")
        , ((modShift, xK_Return),    runInShell "'cf ~'")
        , ((modAlt,   xK_Return),    runInShell "'cf /'")
        , ((modCtrl,  xK_Return),    runInShell "'rcd'")

        -- , ((modShift, xK_c),         recompileXmonad)
        -- , ((modShift, xK_q),         restartXmonad)

        -- Set defined XMonad modes
        , ((modShift, xK_x),         setMode "exit")
        , ((modShift, xK_r),         setMode "resize")
        , ((modShift, xK_p),         setMode "prompt")
        , ((modShift, xK_s),         setMode "search")

        -- MISC
        --
        -- TODO add screen recording with ffmpeg?
        --
        -- Insert: paste xselection
        , ((noMod, xK_Insert),       pasteSelection)
        -- PrintScreen: take a screenshot of entire screen
        , ((noMod, xK_Print),        unGrab *> screenshotCopy FullScreen)
        -- Mod+PrintScreen: take a screenshot of focused window only
        , ((modm, xK_Print),         unGrab *> screenshotCopy FocusedWindow)
        -- ModShift+PrintScreen: select a window to take a screenshot of
        , ((modShift, xK_Print),     unGrab *> screenshotCopy SelectedWindow)
        ]
        ++
        [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip xmWS [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shift)]
        ]
