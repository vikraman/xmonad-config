module Manage where

import           Control.Monad               (liftM2)
import           XMonad
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Scratchpad

import Config
import Utils

myManageHook = (composeAll . concat $
                [ [isDialog       --> doCenterFloat]
                , [className =* c --> doCenterFloat | c <- myCFloats]
                , [title =* t     --> doCenterFloat | t <- myTFloats]
                , [resource =* i  --> doIgnore      | i <- myIgnores]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!0)    | x <- my1Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!1)    | x <- my2Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!2)    | x <- my3Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!3)    | x <- my4Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!4)    | x <- my5Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!5)    | x <- my6Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!6)    | x <- my7Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!7)    | x <- my8Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!8)    | x <- my9Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!9)    | x <- my0Shifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!10)   | x <- myMinusShifts]
                , [(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!11)   | x <- myEqualShifts]
                ]
               ) <+> manageDocks <+> scratchpadManageHook (W.RationalRect 0.1 0.0 0.8 0.5) <+> namedScratchpadManageHook scratchpads
  where
    myCFloats        = ["feh","mplayer","vlc","mpv","Xmessage","Qjackctl","Qsynth"]
    myTFloats        = ["float","roottail","Gnuplot","nam","Nam","exe","Presentation","plugin-container","EdSim"]
    myIgnores        = ["desktop_window"]
    my1Shifts        = []
    my2Shifts        = ["Firefox","Chromium"]
    my3Shifts        = ["Eclipse"]
    my4Shifts        = ["Thunar", "dolphin"]
    my5Shifts        = ["Xchat","Pidgin"]
    my6Shifts        = ["ssh"]
    my7Shifts        = []
    my8Shifts        = []
    my9Shifts        = []
    my0Shifts        = ["vlc"]
    myMinusShifts    = ["Qjackctl","QSynth"]
    myEqualShifts    = ["MPlayer"]
    viewShift        = doF . liftM2 (.) W.greedyView W.shift
