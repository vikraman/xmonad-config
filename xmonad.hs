--
-- ~/.xmonad/xmonad.hs
--
-- vh4x0r's xmonad.hs
--

-- Imports
--
-- Main
import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Actions
import XMonad.Actions.CycleWS

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.Tabbed

-- Layout helpers
import XMonad.Layout.LayoutHints
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell

-- Xmonad utils
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

-- Haskell utils
import System.IO
import Data.List

-- Main
--
main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $ defaultConfig
			{
				modMask		= myModMask,
				terminal	= myTerminal,
				borderWidth	= myBorderWidth,
				workspaces	= myWorkspaces,
				manageHook	= myManageHook,
				layoutHook	= myLayoutHook,
				logHook		= myLogHook xmproc,
				startupHook	= myStartupHook
			}	`additionalKeys` myKeys

-- Teh modMask key
-- 
myModMask = mod4Mask

-- Programs
--
myTerminal	= "Terminal"
myBrowser	= "chromium"
myFilemgr	= "Thunar"

-- Border width
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
			-- program launchers
			((myModMask,	xK_Tab),	spawn myTerminal),
			((myModMask,	xK_p),		spawn (myTerminal ++ " -e ncmpcpp")),
			((myModMask,	xK_t),		spawn myFilemgr),
			((myModMask,	xK_w),		spawn myBrowser),

			-- prompt
			((myModMask,	xK_a),		shellPrompt myXPConfig),

			-- cycle workspaces
			((myModMask,				xK_Right),	nextWS),
			((myModMask,				xK_Left),	prevWS),
			((myModMask .|. shiftMask,	xK_Right),	shiftToNext >> nextWS),
			((myModMask .|. shiftMask,	xK_Left),	shiftToPrev >> prevWS)
		 ]

-- List of workspaces
--
myWorkspaces = ["1:main","2:web","3:code","4:file","5:chat","6:ssh","7:mail","8:mon","9:x","0:y","-","="]

-- Manage
--

-- Better matching using `isInfixOf`
-- | @q =* x@. if @x@ `isInfixOf` the result of @q@, return 'True'.
(=*) :: Eq a => Query [a] -> [a] -> Query Bool
q =* x = fmap (x `isInfixOf`) q

myManageHook = (composeAll . concat $
				[
					[isDialog --> doCenterFloat],
					[className =* c --> doCenterFloat | c <- myCFloats],
					[title =* t --> doCenterFloat | t <- myTFloats],
					[resource =* i --> doIgnore	| i <- myIgnores],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "1:main"	| x <- my1Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "2:web"	| x <- my2Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "3:code"	| x <- my3Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "4:file"	| x <- my4Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "5:chat"	| x <- my5Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "6:ssh"	| x <- my6Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "7:mail"	| x <- my7Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "8:mon"	| x <- my8Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "9:x"		| x <- my9Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "0:y"		| x <- my0Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "-"		| x <- myMinusShifts],
					[(className =* x <||> title =* x <||> resource =* x) --> doShift "="		| x <- myEqualShifts]
				]
			   ) <+> manageDocks
	where
		myCFloats =	["feh","MPlayer","vlc","Gimp","Xmessage"]
		myTFloats =	[]
		myIgnores =	["desktop_window"]
		my1Shifts =	[]
		my2Shifts =	["Firefox","Chromium"]
		my3Shifts =	["Eclipse","emacs"]
		my4Shifts =	["Thunar"]
		my5Shifts =	["Xchat","Pidgin"]
		my6Shifts =	[]
		my7Shifts =	[]
		my8Shifts =	[]
		my9Shifts =	[]
		my0Shifts =	[]
		myMinusShifts =	[]
		myEqualShifts =	[]

-- Layout
--
myLayoutHook =	avoidStruts $
				standardLayouts
	where
		standardLayouts =	tiled ||| Mirror tiled ||| Accordion |||
							trueFull ||| magTiled ||| simpleTabbed

		hinted l = layoutHintsWithPlacement (0.5,0.5) l

		tiled = Tall nmaster delta ratio
		trueFull = noBorders Full
		magTiled = magnifier tiled

		nmaster = 1
		ratio = 9/16
		delta = 3/100

-- Log
--
myLogHook pipe = dynamicLogWithPP $ defaultPP { ppOutput = hPutStrLn pipe }

-- Startup
--
myStartupHook = do
	spawn "pgrep xcompmgr || xcompmgr"
	spawn "feh --bg-fill ~/Pictures/Dropbacks/wallpaper"
	spawn "xsetroot -cursor_name left_ptr"
	spawn "pgrep conky || conky"
