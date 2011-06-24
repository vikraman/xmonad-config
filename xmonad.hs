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

-- myModMask
-- 
myModMask = mod4Mask

-- myTerminal
--
myTerminal = "Terminal"

-- myBorderWidth
--
myBorderWidth = 0

-- myFonts
--
myXFTFont = "xft:Monospace:pixelsize=12"

-- myColors
--
myBGColor		= "#202020"
myPromptFGColor	= "#FF931E"
myPromptBGColor	= "#101010"

-- myXPConfig
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

-- myKeys
--
myKeys = 	[
				((myModMask,	xK_Tab),	spawn myTerminal),
				((myModMask,	xK_p),		spawn (myTerminal ++ " -e ncmpcpp")),
				((myModMask,	xK_a),		shellPrompt myXPConfig)
			]

-- myManageHook
--
myManageHook = (composeAll
   [ className =? "Gimp" --> doFloat ])
   <+> manageDocks <+> manageHook defaultConfig

-- myLayoutHook
--
myLayoutHook = avoidStruts $ layoutHook defaultConfig

-- myStartupHook
--
myStartupHook = do
	spawn "pgrep xcompmgr || xcompmgr"
	spawn "Esetroot -scale ~/Pictures/Dropbacks/wallpaper/wallpaper.jpg"
	spawn "xsetroot -cursor_name left_ptr"
	spawn "pgrep conky || conky"
