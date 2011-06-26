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
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowGo

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.Tabbed

-- Layout helpers
import XMonad.Layout.LayoutHints
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell

-- Xmonad utils
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.XSelection
import qualified XMonad.StackSet as W

-- Haskell utils
import Control.Monad (liftM2)
import Data.List
import System.IO

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
			}	`additionalKeysP` myKeys

-- Teh modMask key
-- 
myModMask = mod4Mask

-- Teh terminal
--
myTerminal = "Terminal"

-- Programs
--
myBrowser = "chromium-browser"

-- Border width
--
myBorderWidth = 0

-- Fonts
--
myFont = "-*-fixed-bold-r-normal-*-12-*-*-*-*-*-*-*"
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
				font = myFont,
				position = Top,
				borderColor = myBGColor,
				height = 18
			}

-- Additional keys
--
myKeys = [
			-- program launchers
			("M-<Tab>",	spawn myTerminal),
			("M-m",		raiseMaybe (runInTerm "-T mutt" "mutt") (title =* "mutt")),
			("M-p",		raiseMaybe (runInTerm "-T ncmpcpp" "ncmpcpp") (title =* "ncmpcpp")),
			("M-t",		spawn "Thunar"),
			("M-w",		raiseBrowser),

			-- multimedia keys
			("M-<XF86AudioMute>",			spawn "amixer -q sset Master toggle"),
			("M-<XF86AudioLowerVolume>",	spawn "amixer -q sset Master 1%-"),
			("M-<XF86AudioRaiseVolume>",	spawn "amixer -q sset Master 1%+"),
			("M-<XF86AudioPrev>",			spawn "mpc prev"),
			("M-<XF86AudioPlay>",			spawn "mpc toggle"),
			("M-<XF86AudioNext>",			spawn "mpc next"),
			("M-S-<XF86AudioPlay>",			spawn "mpc stop"),

			-- prompt
			("M-r",			shellPrompt myXPConfig),
			("M-S-r",		runOrRaisePrompt myXPConfig),

			-- url launcher
			("M-s",			safePromptSelection myBrowser),

			-- cycle workspaces
			("M-<Right>",	moveTo Next NonEmptyWS),
			("M-<Left>",	moveTo Prev NonEmptyWS),
			("M-S-<Right>",	shiftToNext >> nextWS),
			("M-S-<Left>",	shiftToPrev >> prevWS)
		 ]

-- List of workspaces
--
myWorkspaces = ["1:main","2:web","3:code","4:file","5:chat","6:ssh","7:mail","8:mon","9:float","0:x","-","="]

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
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!0)	| x <- my1Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!1)	| x <- my2Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!2)	| x <- my3Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!3)	| x <- my4Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!4)	| x <- my5Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!5)	| x <- my6Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!6)	| x <- my7Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!7)	| x <- my8Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!8)	| x <- my9Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!9)	| x <- my0Shifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!10)	| x <- myMinusShifts],
					[(className =* x <||> title =* x <||> resource =* x) --> viewShift (myWorkspaces!!11)	| x <- myEqualShifts]
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
		my6Shifts =	["ssh"]
		my7Shifts =	["mutt"]
		my8Shifts =	[]
		my9Shifts =	[]
		my0Shifts =	["ncmpcpp","MPlayer","vlc"]
		myMinusShifts =	[]
		myEqualShifts =	[]
		viewShift = doF . liftM2 (.) W.greedyView W.shift

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
myLogHook pipe = dynamicLogWithPP $ defaultPP
					{
						ppOutput = hPutStrLn pipe
					}

-- Startup
--
myStartupHook = do
	spawn "pgrep xcompmgr || xcompmgr"
	spawn "feh --bg-fill ~/Pictures/Dropbacks/wallpaper"
	spawn "xsetroot -cursor_name left_ptr"
	spawn "pgrep conky || conky"
