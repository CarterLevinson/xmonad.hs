module Config (
    xmModes,
    xmStartupHook,
    xmManageHook,
    xmLogHook,
    xmEventHook,
    xmWindowKeys,
    xmWorkspaceKeys,
    xmLauncherKeys,
    xmUtilityKeys,
    module Config.Core,
    module Config.Grids,
    module Config.Queries,
    module Config.Layouts,
    module Config.Modes,
    module Config.StatusBars,
    module Config.Utils,
    module Config.Workspaces,
)
where

import Config.Core
import Config.Grids
import Config.Layouts
import Config.Queries
import Config.Modes
import Config.StatusBars
import Config.Utils
import Config.Workspaces

import qualified Data.Map as M

import Data.Bits ((.|.))
import Data.Monoid (All)
import Graphics.X11
import Graphics.X11.Xlib.Extras (Event)
import System.Exit (exitSuccess)

import XMonad.Config (Default (..))
import XMonad.Core (ManageHook, X, XConfig (..), io, spawn)
import XMonad.Layout (ChangeLayout (..), JumpToLayout (..))
import XMonad.ManageHook (className, title, (=?))
import XMonad.Operations (kill, sendMessage, windows, withFocused)

import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS (Direction1D (..), WSType (..), emptyWS)
import XMonad.Actions.SpawnOn (manageSpawn)

import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.Search as S

import XMonad.Layout.Hidden (hideWindow, popNewestHiddenWindow)
import XMonad.Layout.WindowNavigation (Direction2D (..), Navigate (..))

import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Hooks.Modal (Mode, exitMode, mode, setMode)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste (pasteSelection)
import XMonad.Util.Run
import XMonad.Util.Ungrab (unGrab)

-- import           XMonad.Util.SpawnOnce

xmStartupHook :: X ()
xmStartupHook = mempty

-- use loginctl? proper order to start these programs?
-- seems to work fine
-- start some nice programs
-- xmStartupHook :: X ()
-- xmStartupHook = do
--     spawnOnce $ term ++ " --name weechat -e weechat"
--     spawnOnce $ term ++ " --name neomutt -e neomutt"
--     spawnOnce $ term ++ " --name spotify -e spt"
--     spawnOnce $ term ++ " --name main"

xmLogHook :: X ()
xmLogHook = workspaceHistoryHook

xmEventHook :: Event -> X All
xmEventHook = swallowEventHook (className =? term) (return True)

xmManageHook :: ManageHook
xmManageHook = manageSpawn <> xmManageQueries <> manageHook def


scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "htop" (term ++ "htop") (title =? "htop") defaultFloating
    , NS "paru" (term ++ shell ++ " -is paru") (title =? "paru") defaultFloating
    ]

xmWindowKeys :: [KeyMap]
xmWindowKeys =
    -- Alt+Tab, Alt+Shift+Tab: cycle through focused windows
    [ ((alt, xK_Tab), windows W.focusUp)
    , ((altShift, xK_Tab), windows W.focusDown)
    , -- Layouts

      -- Mod+space: move to next layout in layout hook
      ((modm, xK_space), sendMessage NextLayout)
    , -- Mod+Shift+space: jump to first layout in layout hook
      ((modShift, xK_space), sendMessage $ JumpToLayout "BSP")
    , -- ModShift+t: toggle tabbed layout
      ((modShift, xK_t), sendMessage toggleTabs)
    , -- ModShift+f: toggle full layout
      ((modShift, xK_f), sendMessage toggleFull)
    , -- ModShift+m: toggle magnify window
      ((modShift, xK_m), sendMessage toggleMagnify)
    , -- Toggle gaps

      -- Mod+s: "sinks" floating window back into tiled layout
      ((modm, xK_s), withFocused $ windows . W.sink)
    , -- Mod+t: toggles status bar struts
      ((modm, xK_t), sendMessage ToggleStruts)
    , -- Mod+Plus, Mod+Minus: increase/decrease magnification zoom commands
      ((modm, xK_plus), sendMessage increaseMagnify)
    , ((modm, xK_minus), sendMessage decreaseMagnify)
    , -- Mod+backslash, ModShift+backslash: hide / restore hidden windows
      ((modm, xK_backslash), withFocused hideWindow)
    , ((modShift, xK_backslash), popNewestHiddenWindow)
    ]
        ++ [ ((m .|. modm, k), sendMessage $ f d)
           | (d, k) <- zip [L, D, U, R] [xK_h, xK_j, xK_k, xK_l]
           , (f, m) <- [(Go, noMod), (Swap, shift)]
           ]

xmWorkspaceKeys :: [KeyMap]
xmWorkspaceKeys =
    -- Workspaces
    [ ((modm, xK_n), DO.swapWith Next (Not emptyWS))
    , ((modm, xK_p), DO.swapWith Prev (Not emptyWS))
    , -- Mod+Tab, Mod+Shift+Tab: cycle through populated workspaces
      ((modm, xK_Tab), DO.moveTo Next (Not emptyWS))
    , ((modShift, xK_Tab), DO.moveTo Prev (Not emptyWS))
    , -- Dynamic Workspaces

      -- Mod+a: create and append new named workspace using dmenu
      ((modm, xK_a), appendWorkspace')
    , -- ModShift+a:
      ((modShift, xK_a), addWorkspace')
    , -- Mod+r: rename current workspace using dmenu
      ((modm, xK_r), renameWorkspace')
    , -- Mod+w: select or create workspace using dmenu
      ((modm, xK_w), selectWorkspace')
    , -- ModShift+w: move focused window to new workspace using dmenu
      ((modShift, xK_w), shiftWorkspace)
    , -- ModCtrl+w: copy focused window to new workspace using dmenu
      ((modCtrl, xK_w), copyWorkspace)
    , -- Mod+BackSpace, delete the current workspace
      ((modm, xK_BackSpace), DW.removeWorkspace)
    , -- ModShift+BackSpace, delete workspace selected from dmenu
      ((modShift, xK_BackSpace), removeWorkspace')
    ]
        ++ [ ((m .|. modm, k), DO.withNthWorkspace f i)
           | (i, k) <- zip [0 ..] [xK_1 .. xK_9]
           , (f, m) <- [(W.greedyView, noMod), (W.shift, shift)]
           ]

xmLauncherKeys :: [KeyMap]
xmLauncherKeys =
    -- Program Launchers
    [ ((modm, xK_d), dmenuRun)
    , ((modm, xK_c), gsLaunchDotfiles)
    , ((modm, xK_m), gsLaunchBookmarks)
    , ((modm, xK_o), gsLaunchApps)
    , -- gridselect search instead?

      -- Set defined XMonad modes
      ((modShift, xK_x), setMode "exit")
    , ((modShift, xK_r), setMode "resize")
    , ((modShift, xK_p), setMode "prompt")
    , ((modShift, xK_s), setMode "search")
    , ((modShift, xK_g), setMode "gaps")
    , ((modm, xK_b), unsafeSpawn browser)
    , ((modm, xK_f), runInShell "ranger")
    , ((modm, xK_Return), runInShell "")
    , ((modShift, xK_Return), runInShell "'cf ~'")
    , ((modAlt, xK_Return), runInShell "'cf /'")
    , ((modCtrl, xK_Return), runInShell "'rcd'")
    , -- Program Killers

      -- Mod+q: kill the focused window
      ((modm, xK_q), kill)
    ]

xmUtilityKeys :: [KeyMap]
xmUtilityKeys =
    -- MISC
    -- Insert: paste xselection
    [ ((noMod, xK_Insert), pasteSelection)
    , -- TODO add screen recording with ffmpeg?
      --
      -- PrintScreen: take a screenshot of entire screen
      ((noMod, xK_Print), unGrab *> maim Fullscreen)
    , -- Mod+PrintScreen: take a screenshot of focused window only
      ((modm, xK_Print), unGrab *> maim Focused)
    , -- ModShift+PrintScreen: select a window to take a screenshot of
      ((modShift, xK_Print), unGrab *> maim Selected)
    , ((noMod, f86AudioNext), spawn "playerctl next")
    , ((noMod, f86AudioPrev), spawn "playerctl previous")
    , ((noMod, f86AudioPlay), spawn "playerctl play-pause")
    , ((noMod, f86AudioMute), spawn "pulseaudio-ctl mute")
    , ((noMod, f86AudioLower), spawn "pulseaudio-ctl down")
    , ((noMod, f86AudioRaise), spawn "pulseaudio-ctl up")
    ]

-- Cycle Workspaces and Screens

-- Mod+Up, Mod+Down: cycle through all available workspaces
-- , ((modm, xK_Up),            nextWS)
-- , ((modm, xK_Down),          prevWS)
-- -- Mod+Right, Mod+Left: cycle through all attached screens
-- , ((modm, xK_Right),         nextScreen)
-- , ((modm, xK_Left),          prevScreen)
--
-- -- ModShift+Up, ModShift+Down: shift to next/prev workspace
-- , ((modShift, xK_Up),        shiftToNext)
-- , ((modShift, xK_Down),      shiftToPrev)
-- -- Modshift+Right, ModShift+Left: shift to next/prev screen
-- , ((modShift, xK_Right),     shiftNextScreen)
-- , ((modShift, xK_Left),      shiftPrevScreen)
--
-- -- ModCtrl+Up, ModCtrl+Down: shift and move to next/prev workspace
-- , ((modCtrl, xK_Up),         shiftToNext >> nextWS)
-- , ((modCtrl, xK_Down),       shiftToPrev >> prevWS)
-- -- ModCtrl+Right, ModCtrl+Left: shift and move to next/prev screen
-- , ((modCtrl, xK_Right),      shiftNextScreen >> nextScreen)
-- , ((modCtrl, xK_Left),       shiftPrevScreen >> prevScreen
