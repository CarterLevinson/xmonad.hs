{-# LANGUAGE TupleSections #-}

module XMonad.Config.Modal (xmModes) where

-- import           Core
import           XMonad.Config.Data
import           XMonad.Config.Static
import           XMonad.Config.Util

import qualified Data.Map           as M
import           System.Exit        (exitSuccess)

import           Control.Monad.State
-- import XMonad
import           XMonad.Core
import           XMonad.Layout
import           XMonad.Operations
import           Graphics.X11.Types
import qualified XMonad.StackSet    as W

import           XMonad.Hooks.Modal (Mode, mode, exitMode)


xmModes :: [Mode]
xmModes = [xmExitMode, xmLaunchMode, xmWorkspaceMode, xmResizeMode]

exitMsg :: String
exitMsg = "(l)ock;(p)oweroff;(r)eboot;(s)uspend;(h)ibernate;(e)xit;"

xmExitMode :: Mode
xmExitMode =  mode "exit" $ \_ -> M.fromList lst
    where
        lst :: [KeyMap]
        lst =
          [ ((noMod, xK_l), spawn "xscreensaver-command -lock" >> exitMode)
          , ((noMod, xK_p), spawn "systemctl poweroff")
          , ((noMod, xK_r), spawn "systemctl reboot")
          , ((noMod, xK_s), spawn "systemctl suspend" >> exitMode)
          , ((noMod, xK_h), spawn "systemctl hibernate" >> exitMode)
          , ((noMod, xK_x), io exitSuccess)
          -- , ((shift, xK_r), restart (home ++ "/.xmonad/xmonad-x86-64-linux") True)
          ]

xmLaunchMode :: Mode
xmLaunchMode = mode "launch" $ \_ -> M.fromList lst
  where
    lst :: [KeyMap]
    lst =
      let runExit   str = runInShell str >> exitMode
          spawnExit str = spawn str      >> exitMode
       in [ ((noMod, xK_f), runExit   "ranger")
          , ((noMod, xK_h), runExit   "htop")
          , ((noMod, xK_e), runExit   "nvim /tmp/tmp.txt")
          , ((noMod, xK_c), runExit   "bc -q" )
          , ((noMod, xK_s), spawnExit "spotify-launcher")
          , ((noMod, xK_b), spawnExit "qutebrowser")
          , ((noMod, xK_w), spawnExit "firefox")
          , ((modm, xK_w),  spawnExit "chromium")
          ]

xmResizeMode :: Mode
xmResizeMode = mode "resize" $ \_ -> M.fromList lst
    where
        lst =
          [ ((noMod, xK_Left), sendMessage Expand)
          , ((noMod, xK_Right), sendMessage Shrink)
          -- , ((noMod, xK_Up),  sendMessage MirrorExpand)
          -- , ((noMod, xK_Down), sendMessage MirrorSrhink)
          ]

xmPromptMode :: Mode
xmPromptMode = mode "prompt" $ \_ -> M.fromList lst
    where
        lst = []

-- letter: focus workspace beginning with letter 'a' to 'z'
-- shift+letter: move to workspace beginning with letter 'a' to 'z'
xmWorkspaceMode :: Mode
xmWorkspaceMode =
    let letFocusExit ws = withLetWorkspace W.greedyView ws >> exitMode
        focusExit i     = windows (W.greedyView i) >> exitMode
        focusLetter = zip (map (noMod, ) [xK_a..xK_z])
                          (map letFocusExit ['a'..'z'])
        moveLetter = zip (map (shift, ) [xK_a..xK_z])
                         (map (withLetWorkspace W.shift) ['a'..'z'])
        focusNumber = zip (map (noMod, ) [xK_1..xK_9])
                          (map (focusExit . show ) [1..9])
        moveNumber = zip (map (shift, ) [xK_1..xK_9])
                         (map (windows . W.shift . show) [1..9])
        -- numsMap         = [   (( m .|. noMod, k), windows $ f i)
        --                   |   (i, k) <- zip (map show [1..9]) [xK_1..xK_9]
        --                   ,   (f, m) <- [(W.greedyView, 0), (W.shift, shift)]
        --                   ]
     in mode "workspace" $ \cfg -> M.fromList $
         focusLetter ++
         moveLetter ++
         focusNumber ++
         moveNumber

--refactor
withLetWorkspace :: (String -> WindowSet -> WindowSet) -> Char -> X ()
withLetWorkspace job fstLet = do
  ws      <- gets (map W.tag . W.hidden . windowset)
  current <- gets (W.currentTag . windowset)
  let appJob ws =
        case take 1 $ filter (\w -> fstLet == head w) ws of
          (w:_) -> windows $ job w
          []    -> return ()
   in if head current == fstLet
        then appJob $ filter (/= current) ws
        else appJob ws
