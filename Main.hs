{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- TODO: Finalize Layouts & learn more about layout internals see xmonad.doc
-- TODO: Choose between modal or submap
-- TODO: pretty print dynamic icons to window title not workspace id

module Main (main) where

import Config
import XMonad

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.Modal (modal)
import XMonad.Hooks.StatusBar (withSB)

import XMonad.Util.EZConfig (
    additionalKeys,
    removeKeys,
 )

-------------------------------------------------------------------------------

xmWS :: [WorkspaceId]
xmWS =
    [ "Main"
    , "IRC"
    , "Email"
    , "Music"
    ]

xmModal :: XConfig l -> XConfig l
xmModal = modal xmModes

xmDelKeys :: [KeyPair]
xmDelKeys =
    let mapModPlusShift ks = fmap (modm,) ks ++ fmap (modShift,) ks
        extraKeys = [xK_Return, xK_comma, xK_period, xK_slash, xK_space]
     in mapModPlusShift $ [xK_a .. xK_z] ++ [xK_1 .. xK_9] ++ extraKeys

xmAddKeys :: [KeyMap]
xmAddKeys =
    xmWindowKeys
        ++ xmWorkspaceKeys
        ++ xmLauncherKeys
        ++ xmUtilityKeys

main :: IO ()
main =
    let main' cfg =
            xmonad
                . xmModal
                . ewmh
                . ewmhFullscreen
                . docks
                . withSB (xmobarBot <> xmobarTop)
                $ cfg
                `removeKeys` xmDelKeys
                `additionalKeys` xmAddKeys
     in main'
            def
                { terminal = term
                , borderWidth = 2
                , modMask = modm
                , workspaces = xmWS
                , normalBorderColor = purple
                , focusedBorderColor = gray
                , layoutHook = xmLayoutHook
                , logHook = xmLogHook
                , manageHook = xmManageHook
                , handleEventHook = xmEventHook
                , startupHook = xmStartupHook
                , focusFollowsMouse = False
                , clickJustFocuses = True
                }
