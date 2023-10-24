module XMonad.Config.Workspace
    ( appendWorkspace'
    , addWorkspace'
    , selectWorkspace'
    , renameWorkspace'
    , shiftWorkspace'
    , copyWorkspace'
    ) where

import           XMonad.Core                      (X)
import           XMonad.Operations                (windows)
import           XMonad.StackSet                  (shift)

import           XMonad.Actions.CopyWindow        (copy)
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Config.Prompt             (XPConfig (..), centerPrompt)

withWorkspace' :: String -> (String -> X ()) -> X ()
withWorkspace' s = withWorkspace centerPrompt
    { defaultPrompter = const s }

appendWorkspace', addWorkspace', selectWorkspace', renameWorkspace' :: X ()
appendWorkspace' = appendWorkspacePrompt centerPrompt
    { defaultPrompter = const "Append a New Workspace |>" }

addWorkspace'    = addWorkspacePrompt centerPrompt
    { defaultPrompter = const "Add a New Workspace |>" }

selectWorkspace' = selectWorkspace centerPrompt
    { defaultPrompter = const "Select a Workspace |>" }

renameWorkspace' = renameWorkspace centerPrompt
    { defaultPrompter =  const "Rename the Workspace |>" }

shiftWorkspace', copyWorkspace' :: X ()
shiftWorkspace' = withWorkspace' "Move Window to Workspace |>"
    (windows . shift)

copyWorkspace' = withWorkspace' "Copy Window to Workspace |>"
    (windows . copy)
