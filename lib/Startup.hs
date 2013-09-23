module Startup where

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce

myStartupHook :: X ()
myStartupHook = do
  ewmhDesktopsStartup
  setDefaultCursor xC_left_ptr
  spawnOnce "urxvtd -q -o"
  spawnOnce "taffybar"
  spawnOnce "compton"
  spawn "eval $(cat ~/.fehbg)"
  spawn "xrdb -merge ~/.Xresources"
  spawnOnce "xscreensaver -no-splash"
  spawn "setxkbmap -option ctrl:nocaps"
  spawn "xset b off"
  spawnOnce "gtk-redshift"
  spawnOnce "wpa_gui -t"
