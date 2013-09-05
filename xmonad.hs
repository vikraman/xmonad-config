-- ~/.xmonad/xmonad.hs

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import DBus.Client.Simple

import Config
import Keys
import Layout
import Log
import Manage
import Startup

main :: IO ()
main = do
  client <- connectSession
  xmonad $ withUrgencyHook NoUrgencyHook
    $ defaultConfig { terminal           = myTerminal
                    , modMask            = myModMask
                    , focusFollowsMouse  = myFocusFollowsMouse
                    , clickJustFocuses   = myClickJustFocuses
                    , normalBorderColor  = myNormalBorderColor
                    , focusedBorderColor = myFocusedBorderColor
                    , borderWidth        = myBorderWidth
                    , workspaces         = myWorkspaces
                    , keys               = myKeys
                    , logHook            = myLogHook client
                    , manageHook         = myManageHook
                    , layoutHook         = myLayoutHook
                    , startupHook        = myStartupHook
                    }
