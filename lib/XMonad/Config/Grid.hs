module XMonad.Config.Grid
    ( gsLaunchApps
    , gsLaunchBookmarks
    , gsLaunchDotfiles
    ) where


import           XMonad.Core

import           XMonad.Actions.GridSelect
import           XMonad.Config.Data
import           XMonad.Config.Static

gsConf :: GSConfig String
gsConf =
  def
    { gs_cellheight = 40
    , gs_cellwidth = 200
    , gs_cellpadding = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    -- , gs_font = anonPro
    }

spawnSelected' :: Grid -> X()
spawnSelected' g = gridselect gsConf g >>= flip whenJust spawn

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
gsLaunchApps = spawnSelected' (cliGrid ++ guiGrid)
    where
        cliGrid = makeGrid cliCells (\c -> term ++ " -e " ++ c)
        guiGrid = guiCells
