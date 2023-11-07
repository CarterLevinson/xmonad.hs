module Config.Utils (
    dmenu,
    dmenuMakeArgs,
    dmenuFont,
    dmenuRun,
    dunstify,
    dunstifyArgs,
    confirm,
    runInShell,
    makeFontString,
    scriptify,
    maim,
) where

import Config.Core

import Control.Monad (when)

import XMonad.Core (X)
import XMonad.Util.Run (
    runInTerm,
    safeSpawn,
    safeSpawnProg,
    unsafeSpawn,
 )

import qualified XMonad.Util.Dmenu as D

dunstifyArgs :: [String] -> String -> String -> String
dunstifyArgs = undefined

dunstify :: String -> String -> X ()
dunstify = undefined

-- dunstify msg body = unsafeSpawn $ cmd ++ " '" ++ msg ++ "' '" ++ body ++ "'"
--     where
--         cmd = "dunstify -a xmonad "
--
scriptify :: String -> X ()
scriptify script =
    let dir = source ++ "/scripts/"
     in safeSpawnProg $ dir ++ script

-- scriptify :: String -> String
-- scriptify name = source ++ "/scripts/" ++ name

-- notifyCommand :: Command -> String -> String -> String -> Command
-- notifyCommand cmd msg sbody fbody = cmd
--     ++ " && "
--     ++ dunstify msg sbody
--     ++ " || "
--     ++ dunstify msg fbody
--
-- notifyCommand' :: Command -> String -> Command
-- notifyCommand' cmd msg = notifyCommand cmd msg
--     "Event was successfully completed."
--     "Event has failed and returned an error code"
--
confirm :: String -> X () -> X ()
confirm m f = do
    result <- D.dmenu [m]
    when (init result == m) f

dmenuFont :: XftFont
dmenuFont = firaCode

-- [ "-p",  "Yes, Master?"
-- , "-h",  "24"
-- , "-nb", "#121212"
-- , "-nf", white
-- , "-sb", "#737CA1"
-- , "-sf", "#222330"
-- , "-fn", show dmenuFont
-- , "-b"
-- ]
--
dmenuMakeArgs :: String -> Maybe Int -> [String]
dmenuMakeArgs prompt height =
    [ "-p"
    , prompt
    , "-nb"
    , "#121212"
    , "-nf"
    , white
    , "-sb"
    , "#737CA1"
    , "-sf"
    , white
    , "-fn"
    , show dmenuFont
    ]
        ++ case height of
            Just h ->
                ["-h", show h, "-b"]
            Nothing ->
                ["-h", "24", "-b"]

dmenu :: String -> [String] -> X String
dmenu prompt =
    let args = dmenuMakeArgs prompt Nothing
     in D.menuArgs "dmenu" args

dmenuRun :: X ()
dmenuRun =
    let args = dmenuMakeArgs "Yes, Master?" Nothing
     in safeSpawn "dmenu_run" args

-- dmenuRun :: X ()
-- dmenuRun = unsafeSpawn $ "dmenu_run -p 'Yes, Master?' "
--     ++ "-nb '" ++ "#121212" ++ "' "
--     ++ "-nf '" ++ white     ++ "' "
--     -- ++ "-sb '" ++ "#FA0074" ++ "' "
--     -- ++ "-sb '" ++ "#1200f8" ++ "' "
--     -- ++ "-sb '" ++ "#737CA1" ++ "' "
--     ++ "-sb '" ++ "#222330" ++ "' "
--     ++ "-sf '" ++ white     ++ "' "
--     ++ "-fn '" ++ font      ++ "' "
--     ++ "-h 24 -b"
--     where
--         font = show dmenuFont
--
runInShell :: Command -> X ()
runInShell prog = runInTerm' (shell ++ " -is eval " ++ prog)
  where
    runInTerm' = runInTerm ""

makeFontString :: XftFont -> Font
makeFontString xft = "xft:" ++ show xft

maim :: ScreenshotAction -> X ()
maim ssa =
    case ssa of
        Fullscreen ->
            scriptify "notify-screenshot-full.sh"
        Selected ->
            scriptify "notify-screenshot-selected.sh"
        Focused ->
            scriptify "notify-screenshot-focused.sh"
