-- ~/.xmonad/xmonad.hs

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook

import System.Taffybar.Hooks.PagerHints

import Config
import Keys
import Layout
import Log
import Manage
import Startup

main :: IO ()
main = do xmonad
       $ ewmh
       $ pagerHints
       $ withUrgencyHook NoUrgencyHook
       $ defaultConfig { terminal           = myTerminal
                       , modMask            = myModMask
                       , focusFollowsMouse  = myFocusFollowsMouse
                       , clickJustFocuses   = myClickJustFocuses
                       , normalBorderColor  = myNormalBorderColor
                       , focusedBorderColor = myFocusedBorderColor
                       , borderWidth        = myBorderWidth
                       , workspaces         = myWorkspaces
                       , keys               = myKeys
                       , manageHook         = myManageHook
                       , layoutHook         = myLayoutHook
                       , startupHook        = myStartupHook
                       }
