{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections             #-}

-- TODO: Finalize Layouts & learn more about layout internals see xmonad.doc
-- TODO: Choose between modal or submap
-- TODO: pretty print dynamic icons to window title not workspace id

module Main (main) where

import           Config
import           XMonad

import qualified Data.Map                           as M
import qualified Data.Text                          as T
import qualified XMonad.StackSet                    as W


import qualified XMonad.Actions.Search              as S

import           XMonad.Actions.CycleWS


import           XMonad.Hooks.DynamicIcons          (appIcon, iconsPP)
import           XMonad.Hooks.EwmhDesktops          (ewmh, ewmhFullscreen)
import           XMonad.Hooks.ManageDocks           (ToggleStruts (..), docks)
import           XMonad.Hooks.Modal                 (exitMode, modal, mode,
                                                     setMode)
import           XMonad.Hooks.Script                ()
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP

import           XMonad.Layout.BinarySpacePartition hiding (Swap)
import           XMonad.Layout.Hidden               (hideWindow,
                                                     popNewestHiddenWindow)
import           XMonad.Layout.WindowNavigation     (Navigate (..))

import           XMonad.Util.EZConfig               (additionalKeys, removeKeys)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Paste                  (pasteSelection)
import           XMonad.Util.Ungrab                 (unGrab)
import           XMonad.Util.WorkspaceCompare       (getSortByIndex)

import           System.Exit                        (exitSuccess)

-- import           XMonad.Actions.TopicSpace          as TS
-- import           XMonad.Layout.SubLayouts
-- import           XMonad.Layout.Fullscreen


------------------------------------------------------------------------------

xmPP :: PP
xmPP = def
    { ppCurrent             = xmobarColor "#ECEFF3" "" . wrap "⟪" "⟫"
    , ppVisible             = wrap "⟪" "⟫"
    , ppHidden              = wrap "❲" "❳"
    , ppHiddenNoWindows     = const ""
    , ppSep                 = " | "
    , ppUrgent              = xmobarColor "#FF003F"   "" . wrap "!" "!"
    , ppTitle               = shorten 50
    , ppLayout              = last . words
    , ppExtras              = []
    , ppSort                = getSortByIndex
    -- , ppTitle               = id
    -- , ppHidden           = id
    -- , ppHiddenNoWindows  = const ""
    -- , ppVisibleNoWindows = Nothing
    -- , ppPrinters         = empty
    }

-- TODO: Figure out dynamic icons for focused window title not workspaces
xmIconsPP :: X PP
xmIconsPP = iconsPP icons xmPP
    where
        icons = composeAll
            [ className =? "Firefox"  <||> className =? "firefox"   --> appIcon "\xE745"
            , className =? "Chromium" <||> className =? "chromium"  --> appIcon "\xE743"
            , className =? "Spotify"  <||> className =? "spotify"   --> appIcon "\xF1BC"
            , className =? "kitty"    <||> className =? "alacritty" --> appIcon "\xE795"
            ]


xmobarMain, xmobarTop, xmobarBot :: StatusBarConfig
xmobarTop = statusBarPropTo "_XMONAD_LOG_TOP" "xmobar-top" (pure xmPP)
xmobarBot = statusBarPropTo "_XMONAD_LOG_BOT" "xmobar-bot" (pure xmPP)
xmobarMain = statusBarProp "xmobar-main" (pure xmPP)

-- Replace with xmobar single?
-- xmobar :: StatusBarConfig xmobar =  "_XMONAD_LOG_0" "xmobar" (pure xmPP)

------------------------------------------------------------------------------

restartXmonad = home ++ "/.xmonad/scripts/notify-restart-xmonad.sh"
recompileXmonad = home ++ "/.xmonad/scripts/notifiy-restart-xmonad.sh"
-- restartXmonad :: X ()
-- restartXmonad = spawn $ notifyCommand'
--     "xmonad --restart"
--     "XMonad restarted"

-- recompileXmonad :: X ()
-- recompileXmonad = do
--     spawn $ dunstify
--         "Recompiling XMonad"
--         "Starting recompilation now."
--     spawn $ notifyCommand
--         "xmonad --recompile"
--         "Recompiling XMonad"
--         "XMonad recompilation was a success."
--         "XMonad recompilation has failed with an error code"

reloadXmonad :: X ()
reloadXmonad = spawn recompileXmonad >> spawn restartXmonad

exitXmonad :: X ()
exitXmonad = io exitSuccess


dmenuRun :: X ()
dmenuRun = spawn $ "dmenu_run -p 'Yes, Master?' "
    ++ "-nb '" ++ "#121212" ++ "' "
    ++ "-nf '" ++ white     ++ "' "
    ++ "-sb '" ++ "#FF003F" ++ "' "
    ++ "-sf '" ++ white     ++ "' "
    ++ "-fn '" ++ font      ++ "' "
    ++ "-h 24 -b"
    where
        font = show dmenuFont

-------------------------------------------------------------------------------

xmWS :: [String]
xmWS = map show ([1 .. 9]::[Integer])

xmModal :: XConfig l -> XConfig l
xmModal = modal xmModes
    where
        xmModes =
            [ mode "exit"   $ \_ -> M.fromList exitKeys
            , mode "resize" $ const M.empty
            , mode "prompt" $ const M.empty
            , mode "search" $ const M.empty
            , mode "gaps"   $ const M.empty
            ]


exitKeys :: [KeyMap]
exitKeys =
    [ ((noMod, xK_l), exitMode >> spawn "xscreensaver-command -lock")
    , ((noMod, xK_h), exitMode >> spawn "systemctl hibernate")
    , ((noMod, xK_s), exitMode >> spawn "systemctl suspend")
    , ((noMod, xK_p), exitMode >> spawn "systemctl poweroff")
    , ((noMod, xK_r), exitMode >> spawn "systemctl reboot")
    , ((noMod, xK_x), exitMode >> exitXmonad)
    , ((noMod, xK_q), exitMode >> spawn restartXmonad)
    , ((noMod, xK_c), exitMode >> spawn recompileXmonad)
    , ((shift, xK_q), exitMode >> reloadXmonad)
    ]

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

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "htop" (term ++ "htop") (title =? "htop") defaultFloating
    , NS "paru" (term ++ shell ++ " -is paru") (title =? "paru") defaultFloating
    ]



defaultKeyPairs       = mapKeys
    where mapKeys xs  = mapMod xs  ++ mapModShift xs
          mapMod      = fmap (modm, )
          mapModShift = fmap (modShift, )
          -- extraKeys   = [
          --


main :: IO()
main =
    xmonad
    . xmModal
    . ewmh
    . ewmhFullscreen
    . docks
    . withSB (xmobarBot <> xmobarTop)
    $ def
        { terminal = term
        , borderWidth = 2
        , modMask = modm
        , workspaces = xmWS
        , normalBorderColor = purple
        , focusedBorderColor = gray
        , layoutHook = xmLayoutHook
        , logHook = xmLogHook
        , manageHook = xmManageHook <+> manageHook def
        , handleEventHook = xmEventHook
        , startupHook = xmStartupHook
        , focusFollowsMouse = False
        , clickJustFocuses = True
        }
    `removeKeys` defaultKeyPairs
            ( [xK_a .. xK_z]
            ++
            [xK_1 .. xK_9]
            ++
            [ xK_Return
            , xK_comma
            , xK_period
            , xK_slash
            , xK_space
            ] )
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

        -- Mod+a: create and append new named workspace using dmenu
        , ((modm, xK_a),             appendWorkspace')

        -- ModShift+a:
        , ((modShift, xK_a),         addWorkspace')

        -- Mod+r: rename current workspace using dmenu
        , ((modm, xK_r),             renameWorkspace')

        -- Mod+w: select or create workspace using dmenu
        , ((modm, xK_w),             selectWorkspace')

        -- ModShift+w: move focused window to new workspace using dmenu
        , ((modShift, xK_w),         shiftWorkspace)

        -- ModCtrl+w: copy focused window to new workspace using dmenu
        , ((modCtrl, xK_w),          copyWorkspace)

        -- Mod+BackSpace, delete the current workspace
        , ((modm, xK_BackSpace),     removeWorkspace)
        -- ModShift+BackSpace, delete workspace selected from dmenu

        -- Program Killers

        -- Mod+q: kill the focused window
        , ((modm, xK_q),             kill)

        -- Program Launchers

        , ((modm, xK_d),             dmenuRun)

        , ((modm, xK_c),             gsLaunchDotfiles)
        , ((modm, xK_m),             gsLaunchBookmarks)
        , ((modm, xK_o),             gsLaunchApps)
        -- gridselect search instead?

        , ((modm, xK_b),             spawn browser)
        , ((modm, xK_f),             runInShell "ranger")

        , ((modm,     xK_Return),    runInShell "")
        , ((modShift, xK_Return),    runInShell "'cf ~'")
        , ((modAlt,   xK_Return),    runInShell "'cf /'")
        , ((modCtrl,  xK_Return),    runInShell "'rcd'")

        -- Set defined XMonad modes
        , ((modShift, xK_x),         setMode "exit")
        , ((modShift, xK_r),         setMode "resize")
        , ((modShift, xK_p),         setMode "prompt")
        , ((modShift, xK_s),         setMode "search")
        , ((modShift, xK_g),         setMode "gaps")

        -- MISC

        -- TODO add screen recording with ffmpeg?

        -- Insert: paste xselection
        , ((noMod, xK_Insert),       pasteSelection)

        -- PrintScreen: take a screenshot of entire screen
        , ((noMod, xK_Print),        unGrab *> (spawn . ssCopy $ Fullscreen))

        -- Mod+PrintScreen: take a screenshot of focused window only
        , ((modm, xK_Print),         unGrab *> (spawn . ssCopy $ Focused))

        -- ModShift+PrintScreen: select a window to take a screenshot of
        , ((modShift, xK_Print),     unGrab *> (spawn . ssCopy $ Selected))

        , ((noMod, f86AudioNext),    spawn "playerctl next")
        , ((noMod, f86AudioPrev),    spawn "playerctl previous")
        , ((noMod, f86AudioPlay),    spawn "playerctl play-pause")
        , ((noMod, f86AudioMute),    spawn "pulseaudio-ctl mute")
        , ((noMod, f86AudioLower),   spawn "pulseaudio-ctl down")
        , ((noMod, f86AudioRaise),   spawn "pulseaudio-ctl up")
        ]
        ++
        [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip xmWS [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, noMod), (W.shift, shift)]
        ]
        -- ++
        -- [ ((m .|. modm, k), sendMessage $ f d)
        -- | (i, k) <- zip [xK_h, xK_j, xK_k, xK_l] [L, D, U, R]
        -- , (f, m) <- [(Go d, noMod), (Swap d, shift)]
        -- ]
