{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
module Layout where

import Data.Typeable
import XMonad                              (Window)
import XMonad.Hooks.ManageDocks
import XMonad.Layout                       hiding ((|||))
import XMonad.Layout.Accordion
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import Config

data DECORATION = DECORATION deriving (Read, Show, Eq, Typeable)
instance Transformer DECORATION Window where
  transform DECORATION x k = k (noFrillsDeco shrinkText myTheme x) (\(ModifiedLayout _ x') -> x')

data BORDERS = BORDERS deriving (Read, Show, Eq, Typeable)
instance Transformer BORDERS Window where
  transform BORDERS x k = k (withBorder 1 x) (\(ModifiedLayout _ x') -> x')

myLayoutHook = avoidStruts
               $ windowNavigation
               $ mkToggle (NBFULL ?? MIRROR ?? BORDERS ?? DECORATION ?? EOT)
               $ onWorkspace (myWorkspaces!!1) themedTabbed
               $ onWorkspace (myWorkspaces!!8) float
               $ standardLayouts
  where
    standardLayouts    = tiled ||| themedTabbed ||| magTiled ||| Accordion ||| float
    themedTabbed       = tabbed shrinkText myTheme
    hinted             = layoutHintsWithPlacement (0.5,0.5)
    tiled              = Tall nmaster delta ratio
    magTiled           = magnifier tiled
    float              = simpleFloat' shrinkText myTheme
    nmaster            = 1
    ratio              = 9/16
    delta              = 3/100
