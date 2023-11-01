module Config.Workspaces
    ( appendWorkspace'
    , addWorkspace'
    , selectWorkspace'
    , renameWorkspace'
    , removeWorkspace'
    , withWorkspace'
    , shiftWorkspace
    , copyWorkspace
    , removeWorkspace
    ) where

import           Config.Utils

import           XMonad                           (gets)
import           XMonad.Core                      (WorkspaceId, X, windowset)
import           XMonad.Operations                (windows)

import qualified XMonad.StackSet                  as W
import qualified XMonad.Actions.DynamicWorkspaces as DW

import           XMonad.Actions.CopyWindow        (copy)

import           XMonad.Util.WorkspaceCompare     (getSortByIndex)

-- import qualified XMonad.Util.Dmenu as D

getTagSortedWS :: X [WorkspaceId]
getTagSortedWS = do
    ws   <- gets (W.workspaces . windowset)
    sort <- getSortByIndex
    return (map W.tag $ sort ws)

dmenuWorkspace :: String -> X WorkspaceId
dmenuWorkspace prompt = getTagSortedWS
    >>= dmenu prompt

selectWorkspace' :: X ()
selectWorkspace' = do
    choice <- dmenuWorkspace "Which workspace, Master?"
    set    <- gets windowset
    if not $ W.tagMember choice set
       then DW.addWorkspace choice
       else windows $ W.greedyView choice

appendWorkspace' :: X ()
appendWorkspace' = dmenuWorkspace "Append WS :>"
    >>= DW.appendWorkspace

addWorkspace' :: X ()
addWorkspace' = dmenuWorkspace "Add WS :>"
    >>= DW.addWorkspace

renameWorkspace' :: X ()
renameWorkspace' = dmenu "Rename WS :>" []
    >>= DW.renameWorkspaceByName

removeWorkspace' :: X ()
removeWorkspace' = dmenuWorkspace "Delete a workspace :>"
    >>= DW.removeWorkspaceByTag

withWorkspace' :: String -> (String -> X ()) -> X ()
withWorkspace' prompt job = do
     ts <- getTagSortedWS
     let job' t | t `elem` ts = job t
                | otherwise = DW.addHiddenWorkspace t >> job t
     dmenu prompt ts >>= job'

shiftWorkspace :: X ()
shiftWorkspace = withWorkspace' "Which window to move, Master?"
    (windows . W.shift)

copyWorkspace :: X ()
copyWorkspace = withWorkspace' "Which window to copy, Master?"
    (windows . copy)

removeWorkspace :: X ()
removeWorkspace = DW.removeWorkspace
