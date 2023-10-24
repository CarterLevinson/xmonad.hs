module XMonad.Config.Util
    ( runInShell
    , map2
    -- , spawnSelected'
    , makeFontString
    ) where

import qualified Data.Text as T
-- import qualified XMonad.Actions.GridSelect as GS

import           XMonad.Config.Data
import           XMonad.Config.Static

import           XMonad.Core     (X, spawn, whenJust)
import           XMonad.Util.Run (runInTerm)

-- import           XMonad.Util.Dmenu (menuArgs)
-- import           XMonad.Util.Dzen
--
runInShell :: Command -> X()
runInShell prog = runInTerm' (shell ++ " -is eval " ++ prog)
    where
        runInTerm' = runInTerm ""

map2 :: [x] -> (x -> y) -> (x -> y) -> [y]
map2 xs f g = map f xs ++ map g xs

-- showLower :: a -> String
-- showLower = T.toLower . T.toText . show


toLower' :: String -> String
toLower' = T.unpack . T.toLower . T.pack

makeFontString :: XFTFont -> Font
makeFontString xft = "xft:" ++ family xft ++ "-" ++ (show . size $ xft)
    ++ ":weight=" ++ (toLower' . show . weight $ xft)
    ++ ":slant=" ++ (toLower' . show . slant $ xft)
    ++ ":spacing=" ++ (toLower' . show . slant $ xft)
    ++ ":hinting=" ++ (toLower' . show . hinting $ xft)
    ++ ":antialias=" ++ (toLower' . show . antialias $ xft)


