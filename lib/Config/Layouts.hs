{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Config.Layouts (
    xmLayoutHook,
    toggleTabs,
    toggleFull,
    toggleMagnify,
    increaseMagnify,
    decreaseMagnify,
) where

import Graphics.X11 (Window)

import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Hooks.ManageDocks (avoidStruts)

import XMonad.Layout (Full (..))
import XMonad.Layout.LayoutCombinators ((|||))

import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Mosaic (mosaic)
import XMonad.Layout.Spiral (spiral)

-- import           XMonad.Layout.Renamed
-- import           XMonad.Layout.SubLayouts
-- import           XMonad.Layout.Fullscreen
import XMonad.Layout.Hidden (hiddenWindows)
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.TabBarDecoration (simpleTabBar)
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation (windowNavigation)

import XMonad.Layout.Magnifier as MAG
import XMonad.Layout.MultiToggle as MT

data FullTransformer = FULL
    deriving (Read, Show, Eq)

instance Transformer FullTransformer Window where
    transform _ x k = k Full (const x)

data TabsTransformer = TABS
    deriving (Read, Show, Eq)

instance Transformer TabsTransformer Window where
    transform _ x k =
        k
            (simpleTabBar x)
            (\(ModifiedLayout _ (ModifiedLayout _ x')) -> x')

data NoBordersTransformer = NOBORDERS
    deriving (Read, Show, Eq)

instance Transformer NoBordersTransformer Window where
    transform _ x k = k (noBorders x) (\(ModifiedLayout _ x') -> x')

xmLayoutHook =
    mouseResize
        . windowArrange
        . windowNavigation
        . avoidStruts
        . magnifierOff
        . hiddenWindows
        . smartBorders
        . mkToggleFull
        . mkToggleTabbed
        $ layouts
  where
    mkToggleFull = mkToggle (NOBORDERS ?? FULL ?? EOT)
    mkToggleTabbed = mkToggle (single TABS)

-- Note that each layout is separated by ||| which denotes layout choice.
layouts =
    emptyBSP
        ||| spiral (7 / 6)
        ||| mosaic 2 [3, 2]

-- toggle: gaps and toggle tabs?

toggleFull, toggleTabs :: Toggle Window
toggleFull = MT.Toggle FULL
toggleTabs = MT.Toggle TABS

data X where
    X :: X
    Y :: X

-- data MyToggles where
-- MAG.ToggleMagnify :: MyToggles
-- data ToggleMagnify where

toggleMagnify = MAG.Toggle

increaseMagnify, decreaseMagnify :: MagnifyMsg
increaseMagnify = MagnifyMore
decreaseMagnify = MagnifyLess

-- data LayoutToggle
--     = toggleMagnify
-- renameLayouts = renamed [Replace "BSP", Replace "Spiral", Replace "Mosaic"]
-- replaceLayoutNames ls = renamed
--     [ Replace "BSP"
--     , Replace "Spiral"
--     , Replace "Mosaic"
--     ]
--     l
