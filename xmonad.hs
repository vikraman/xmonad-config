-- ~/.xmonad/xmonad.hs

import XMonad
import XMonad.Hooks.UrgencyHook

import System.Taffybar.Hooks.PagerHints

import Config
import HandleEvent
import Keys
import Layout
import Log
import Manage
import Startup

main :: IO ()
main = do xmonad
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
                       , logHook            = myLogHook
                       , manageHook         = myManageHook
                       , layoutHook         = myLayoutHook
                       , startupHook        = myStartupHook
                       , handleEventHook    = myHandleEventHook
                       }
