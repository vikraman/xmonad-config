module Config where

import XMonad
import XMonad.Util.Themes

myTerminal = "urxvtc"
myBrowser = "firefox"

myClickJustFocuses :: Bool
myClickJustFocuses = False

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 0

myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = zipWith (++) (map ((++ ":") . show) [1..])
               ["main","web","code","file","chat","ssh","mail","mon","float","misc","-","="]

myNormalBorderColor  = myBGColor
myFocusedBorderColor = myFocusedColor

myFont             = "-*-fixed-bold-r-normal-*-12-*-*-*-*-*-iso10646-*"

myXFTFont          = "xft:Source Code Pro:SemiBold:size=12:antialias=true"

myBGColor          = "#202020"
myPromptFGColor    = "#FF931E"
myPromptBGColor    = "#101010"
myFocusedColor     = "#3FA9F5"

myTheme = theme robertTheme
