module Config.StatusBars (
    xmobarSingle,
    xmobarBot,
    xmobarTop,
    xmPP,
    xmIconsPP,
)
where

import XMonad.Core (X)
import XMonad.ManageHook (
    className,
    composeAll,
    (-->),
    (<||>),
    (=?),
 )

import XMonad.Hooks.DynamicIcons (appIcon, iconsPP)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import qualified XMonad.Actions.DynamicWorkspaceOrder as DO

------------------------------------------------------------------------------

xmPP :: PP
xmPP =
    def
        { ppCurrent = xmobarColor "#ECEFF3" "" . wrap "⟪" "⟫"
        , ppVisible = wrap "⟪" "⟫"
        , ppHidden = wrap "❲" "❳"
        , ppHiddenNoWindows = const ""
        , ppSep = " | "
        , ppUrgent = xmobarColor "#FF003F" "" . wrap "!" "!"
        , ppTitle = shorten 50
        , ppLayout = last . words
        , ppExtras = []
        , ppSort = DO.getSortByOrder
        , ppVisibleNoWindows = Nothing
        -- , ppPrinters            = []
        }

-- TODO: Figure out dynamic icons for focused window title not workspaces
-- icons :: Query [String]
icons =
    composeAll
        [ className =? "Firefox" <||> className =? "firefox" --> appIcon "\xE745"
        , className =? "Chromium" <||> className =? "chromium" --> appIcon "\xE743"
        , className =? "Spotify" <||> className =? "spotify" --> appIcon "\xF1BC"
        , className =? "kitty" <||> className =? "alacritty" --> appIcon "\xE795"
        ]

xmIconsPP :: PP -> X PP
xmIconsPP = iconsPP icons

xmobarSingle :: StatusBarConfig
xmobarSingle = statusBarPropTo "_XMONAD_LOG" "xmobar-single" (pure xmPP)

xmobarTop :: StatusBarConfig
xmobarTop = statusBarPropTo "_XMONAD_LOG_TOP" "xmobar-top" (pure xmPP)

xmobarBot :: StatusBarConfig
xmobarBot = statusBarPropTo "_XMONAD_LOG_BOT" "xmobar-bot" (pure xmPP)

------------------------------------------------------------------------------
