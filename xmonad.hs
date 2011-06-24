--
-- ~/.xmonad/xmonad.hs
--
-- vh4x0r's xmonad.hs
--

-- Imports
--
-- Main
import XMonad
import XMonad.Hooks.ManageDocks

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell

-- Utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

-- System
import System.IO

-- Main
--
main = do
	xmonad $ defaultConfig
			{
				modMask		= myModMask,
				terminal	= myTerminal,
				borderWidth	= myBorderWidth,
				manageHook	= myManageHook,
				layoutHook	= myLayoutHook,
				startupHook	= myStartupHook
			}	`additionalKeys` myKeys

-- Teh modMask key
-- 
myModMask = mod4Mask

-- Programs
--
myTerminal = "Terminal"
myBrowser = "chromium"
myFilemgr = "Thunar"

-- BorderWidth
--
myBorderWidth = 0

-- Fonts
--
myXFTFont = "xft:Monospace:pixelsize=12"

-- Colors
--
myBGColor		= "#202020"
myPromptFGColor	= "#FF931E"
myPromptBGColor	= "#101010"

-- Prompt theme
--
myXPConfig = defaultXPConfig
			{
				fgColor	= myPromptFGColor,
				bgColor	= myPromptBGColor,
				fgHLight = myPromptBGColor,
				bgHLight = myPromptFGColor,
				promptBorderWidth = 1,
				historySize = 100,
				font = myXFTFont,
				position = Top,
				borderColor = myBGColor,
				height = 18
			}

-- Additional keys
--
myKeys = [
			((myModMask,	xK_Tab),	spawn myTerminal),
			((myModMask,	xK_a),		shellPrompt myXPConfig),
			((myModMask,	xK_p),		spawn (myTerminal ++ " -e ncmpcpp")),
			((myModMask,	xK_t),		spawn myFilemgr),
			((myModMask,	xK_w),		spawn myBrowser)
		 ]

-- Manage
--
myManageHook = (composeAll
				[ className =? "Gimp" --> doFloat ]
			   ) <+> manageDocks <+> manageHook defaultConfig

-- Layout
--
myLayoutHook = avoidStruts $ layoutHook defaultConfig

-- Startup
--
myStartupHook = do
	spawn "pgrep xcompmgr || xcompmgr"
	spawn "Esetroot -scale ~/Pictures/Dropbacks/wallpaper/wallpaper.jpg"
	spawn "xsetroot -cursor_name left_ptr"
	spawn "pgrep conky || conky"
