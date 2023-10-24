module XMonad.Config.Action
    ( gsLaunchDotfiles
    , gsLaunchApps
    , gsLaunchBookmarks
    )
    where

import XMonad.Actions.GridSelect as GS
import XMonad.Core

import XMonad.Config.Static
import XMonad.Config.Data
import XMonad.Config.Util

makeGrid :: Grid -> (String -> String) -> Grid
makeGrid grid f =
    let (ks, vs) = unzip grid
     in zip ks (map f vs)

gsLaunchDotfiles :: X()
gsLaunchDotfiles = spawnSelected' dfGrid
    where
        dfGrid = makeGrid dfCells (\p -> term ++ " -e " ++ editor ++ " " ++ p)

gsLaunchBookmarks :: X()
gsLaunchBookmarks = spawnSelected' bmGrid
    where
        bmGrid = makeGrid bmCells (\u -> browser ++ " " ++ "https://" ++ u)

gsLaunchApps :: X()
gsLaunchApps = spawnSelected' $ cliGrid ++ guiGrid
    where
        cliGrid = makeGrid cliCells (\c -> term ++ " -e " ++ c)
        guiGrid = guiCells

gsFont :: Font
gsFont = makeFontString XFTFont
    { family = "Anonymous Pro"
    , size   = 18
    , slant  = Roman
    , weight = Bold
    , spacing = Proportional
    , hinting = True
    , antialias = True
    }

spawnSelected' :: Grid -> X()
spawnSelected' g = GS.gridselect gsConf g >>= flip whenJust spawn
    where
        gsConf = GS.def
            { GS.gs_cellheight = 40
            , GS.gs_cellwidth  = 200
            , GS.gs_cellpadding = 6
            , GS.gs_originFractX = 0.5
            , GS.gs_originFractY = 0.5
            , GS.gs_font = gsFont
            }
