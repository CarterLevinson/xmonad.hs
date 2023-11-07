module Config.Grids (
    gsLaunchDotfiles,
    gsLaunchApps,
    gsLaunchBookmarks,
)
where

import Config.Core
import Config.Utils

import XMonad.Actions.GridSelect as GS
import XMonad.Core

makeGrid :: Grid -> (String -> String) -> Grid
makeGrid grid f =
    let (ks, vs) = unzip grid
     in zip ks (map f vs)

gsLaunchDotfiles :: X ()
gsLaunchDotfiles = spawnSelected' dfGrid
  where
    dfGrid = makeGrid dfCells (\p -> term ++ " -e " ++ editor ++ " " ++ p)

gsLaunchBookmarks :: X ()
gsLaunchBookmarks = spawnSelected' bmGrid
  where
    bmGrid = makeGrid bmCells (\u -> browser ++ " " ++ "https://" ++ u)

gsLaunchApps :: X ()
gsLaunchApps = spawnSelected' $ cliGrid ++ guiGrid
  where
    cliGrid = makeGrid cliCells (\c -> term ++ " -e " ++ c)
    guiGrid = guiCells

gsFont :: Font
gsFont =
    makeFontString
        firaCode
            { size = 18.0
            , slant = Roman
            , spacing = Proportional
            }

spawnSelected' :: Grid -> X ()
spawnSelected' g = GS.gridselect gsConf g >>= flip whenJust spawn
  where
    gsConf =
        GS.def
            { GS.gs_cellheight = 40
            , GS.gs_cellwidth = 200
            , GS.gs_cellpadding = 6
            , GS.gs_originFractX = 0.5
            , GS.gs_originFractY = 0.5
            , GS.gs_font = gsFont
            }
