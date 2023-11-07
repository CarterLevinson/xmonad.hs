module Config.Workspaces (
    appendWorkspace',
    addWorkspace',
    selectWorkspace',
    renameWorkspace',
    removeWorkspace',
    withWorkspace',
    shiftWorkspace,
    copyWorkspace,
    getTagSortedWS,
    getFirstWS,
) where

import Config.Utils

import Control.Monad (when)
import Control.Monad.State (gets)

import XMonad.Core (
    WorkspaceId,
    X,
    windowset,
 )
import XMonad.Operations (windows)
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (copy)
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.DynamicWorkspaces as DW

-- import           XMonad.Util.WorkspaceCompare         (getSortByIndex)

-- import qualified XMonad.Util.Dmenu as D

getTagSortedWS :: X [WorkspaceId]
getTagSortedWS = do
    ws <- gets (W.workspaces . windowset)
    sort <- DO.getSortByOrder
    return (map W.tag $ sort ws)

getFirstWS :: X WorkspaceId
getFirstWS = head <$> getTagSortedWS

dmenuWorkspace :: String -> X WorkspaceId
dmenuWorkspace prompt = getTagSortedWS >>= dmenu prompt

whenNotEmpty :: (String -> X ()) -> String -> X ()
whenNotEmpty f s = when (s /= "") $ f s

selectWorkspace' :: X ()
selectWorkspace' = do
    choice <- dmenuWorkspace "Which workspace, Master?"
    set <- gets windowset
    if W.tagMember choice set
        then windows $ W.greedyView choice
        else whenNotEmpty DW.addWorkspace choice

appendWorkspace' :: X ()
appendWorkspace' =
    dmenuWorkspace "Append workspace: "
        >>= whenNotEmpty DW.appendWorkspace

-- >>= DW.appendWorkspace

addWorkspace' :: X ()
addWorkspace' =
    dmenuWorkspace "Add workspace: "
        >>= whenNotEmpty DW.addWorkspace

renameWorkspace' :: X ()
renameWorkspace' =
    dmenu "Rename workspace: " []
        >>= whenNotEmpty DW.renameWorkspaceByName

removeWorkspace' :: X ()
removeWorkspace' =
    dmenuWorkspace "Delete a workspace :>"
        >>= DW.removeWorkspaceByTag

withWorkspace' :: String -> (String -> X ()) -> X ()
withWorkspace' prompt job = do
    ts <- getTagSortedWS
    let job' t
            | t `elem` ts = job t
            | otherwise = DW.addHiddenWorkspace t >> job t
    dmenu prompt ts >>= whenNotEmpty job'

shiftWorkspace :: X ()
shiftWorkspace =
    withWorkspace'
        "Move focused workspace to which window, Master?"
        (windows . W.shift)

copyWorkspace :: X ()
copyWorkspace =
    withWorkspace'
        "Which window to copy, Master?"
        (windows . copy)

-- removeWorkspace :: X ()
-- removeWorkspace = DW.removeWorkspace
