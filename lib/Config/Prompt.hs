module Config.Prompt
    ( centerPrompt
    , bottomPrompt
    , topPrompt
    , zshShellPrompt
    , xmonadCmdPrompt
    , XPConfig(..)
    )
    where

import           XMonad.Core

import           Config.Data
import           Config.Static
import           Config.Utils

import           XMonad.Prompt           as P

import           XMonad.Prompt.Man
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.Unicode
import           XMonad.Prompt.Window
import           XMonad.Prompt.Workspace
import           XMonad.Prompt.XMonad
import           XMonad.Prompt.Zsh

-- make bw prompt?
-- import XMonad.Prompt.Pass


promptFont :: Font
promptFont = makeFontString XFTFont
    -- { family    = "Anonymous Pro"
    { family    = "Fira Code"
    , size      = 18
    , slant     = Roman
    , weight    = Bold
    -- , spacing   = Proportional
    , spacing   = Mono
    , hinting   = True
    , antialias = True
    }

promptConfig :: XPConfig
promptConfig = P.def
    { font              = promptFont
    , bgColor           = black
    , fgColor           = purple
    , borderColor       = purple
    , promptBorderWidth = 2
    -- , P.position          = P.CenteredAt (1/2) (1/2)
    , height            = 50
    , autoComplete      = Just 1
    -- , maxComplRows
    }

centerPrompt, bottomPrompt, topPrompt :: XPConfig
centerPrompt = promptConfig
    { P.position = P.CenteredAt (1/2) (1/2) }

bottomPrompt = promptConfig
    { P.position = P.Bottom }

topPrompt    = promptConfig
    { P.position = P.Top }

zshShellPrompt :: X ()
zshShellPrompt = zshPrompt centerPrompt (home ++ "/.xmonad/scripts/capture.zsh")

xmonadCmdPrompt :: X ()
xmonadCmdPrompt = xmonadPrompt centerPrompt

-- centerPrompt :: P.XPConfig
-- centerPrompt =
--   P.def
--     { P.font              = anonPro
--     , P.bgColor           = black
--     , P.fgColor           = purple
--     , P.borderColor       = purple
--     , P.promptBorderWidth = 2
--     , P.position          = P.CenteredAt (1/2) (1/2)
--     , P.height            = 50
--     , P.autoComplete      = Just 1
--     }

-- bottomPrompt :: P.XPConfig
-- bottomPrompt =
--   P.def
--     { P.font              = anonPro
--     , P.bgColor           = black
--     , P.fgColor           = white
--     , P.borderColor       = purple
--     , P.promptBorderWidth = 1
--     , P.position          = P.Bottom
--     , P.height            = 20
--     , P.autoComplete      = Just 1
--     }
