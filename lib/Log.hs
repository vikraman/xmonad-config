module Log where

import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Scratchpad

myLogHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace
