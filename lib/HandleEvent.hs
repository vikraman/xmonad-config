module HandleEvent where

import XMonad
import XMonad.Hooks.EwmhDesktops

myHandleEventHook = fullscreenEventHook <+> ewmhDesktopsEventHook
