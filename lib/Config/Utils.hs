module Config.Utils
    ( dmenu
    , dmenuMakeArgs
    , dmenuFont
    , dunstify
    , notifyCommand
    , notifyCommand'
    , confirm
    , runInShell
    , makeFontString
    , ssCopy
    ) where

import           Config.Data
import           Config.Static

import           Control.Monad     (when)

import           XMonad.Core       (X)
import           XMonad.Util.Run   (runInTerm)

import qualified XMonad.Util.Dmenu as D


dunstifyArgs :: [String] -> String -> String -> String
dunstifyArgs = undefined

dunstify :: String -> String -> String
dunstify msg body = cmd ++ " " ++ "'" ++ msg ++ "' '" ++ body ++ "'"
    where
        cmd = "dunstify -i " ++ home ++ "/.xmonad/icons/haskell-logo.svg"


notifyCommand :: Command -> String -> String -> String -> Command
notifyCommand cmd msg sbody fbody = cmd
    ++ " && "
    ++ dunstify msg sbody
    ++ " || "
    ++ dunstify msg fbody

notifyCommand' :: Command -> String -> Command
notifyCommand' cmd msg = notifyCommand cmd msg
    "Event was successfully completed."
    "Event has failed and returned an error code"

confirm :: String -> X () -> X ()
confirm m f = do
    result <- D.dmenu [m]
    when (init result == m) f

dmenuFont :: XftFont
dmenuFont = firaCode

dmenuMakeArgs :: String -> Maybe Int -> [String]
dmenuMakeArgs prompt height =
    [ "-p",  prompt
    , "-nb", "#121212"
    , "-nf", white
    -- , "-sb", "#FF1744" -- torch red
    , "-sb", "#FF003F"  -- electric crimson
    , "-sf", white
    , "-fn", show dmenuFont
    ]
    ++ case height of
         Just h ->
             [ "-h", show h, "-b" ]
         Nothing ->
             [ "-h", "24",   "-b" ]

dmenu :: String -> [String] -> X String
dmenu prompt = D.menuArgs "dmenu" args
    where args = dmenuMakeArgs prompt Nothing

runInShell :: Command -> X()
runInShell prog = runInTerm' (shell ++ " -is eval " ++ prog)
    where
        runInTerm' = runInTerm ""

makeFontString :: XftFont -> Font
makeFontString xft = "xft:" ++ show xft

maim :: ScreenshotAction -> Command
maim ssa = case ssa of
             Fullscreen ->
                 "maim -q -m 10"
             Selected   ->
                 "maim -q -m 10 -s"
             Focused    ->
                 "maim -q -m 10 -i $(xdotool getactivewindow)"

ssCopy :: ScreenshotAction -> Command
ssCopy ssa =
    let file = "maim-screenshot-$(date '+%m-%d-%Y-T-%T').png"
        clip = "xclip -selection clipboard -t image/png"
        path = "~/pictures/screenshots/"
        pipe = maim ssa ++ " | tee " ++ path ++ file ++ " | " ++ clip
     in notifyCommand pipe "Screenshot"
        ("Screenshot saved to " ++ path ++ ".")
        "Failed to save screenshot."
