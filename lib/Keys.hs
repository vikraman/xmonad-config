module Keys where

import Data.List
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.XSelection

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import Config
import Layout
import Utils

myKeys conf = mkKeymap conf $
              [ ("M-<Tab>",     spawn $ terminal conf)
              , ("M-d",         spawn "dmenu_run")
              , ("M-S-q",       kill)
              , ("M-x",         shellPrompt myXPConfig)
              , ("M-s",         namedScratchpadAction scratchpads "tmux")
              , ("M-h",         namedScratchpadAction scratchpads "htop")
              , ("M-m",         namedScratchpadAction scratchpads "mutt")
              , ("M-p",         namedScratchpadAction scratchpads "ncmpcpp")
              , ("M-a",         namedScratchpadAction scratchpads "alsamixer")

              , ("M-w",         raiseBrowser)
              , ("M-C-l",       spawn "xscreensaver-command -lock")
              , ("M-v",         spawn "feh -rz --bg-fill ~/Pictures/Dropbacks/Wallpaper")

              , ("<XF86MonBrightnessUp>",    spawn "xbacklight +2")
              , ("<XF86MonBrightnessDown>",  spawn "xbacklight -2")

              , ("<XF86AudioLowerVolume>",   spawn "pactl -- set-sink-volume 0 -1%")
              , ("<XF86AudioRaiseVolume>",   spawn "pactl -- set-sink-volume 0 +1%")
              , ("<XF86AudioMute>",          spawn "pactl -- set-sink-mute 0 1")
              , ("S-<XF86AudioMute>",        spawn "pactl -- set-sink-mute 0 0")

              , ("<XF86AudioPrev>",          spawn "mpc -q prev")
              , ("<XF86AudioPlay>",          spawn "mpc -q toggle")
              , ("<XF86AudioNext>",          spawn "mpc -q next")
              , ("S-<XF86AudioPlay>",        spawn "mpc -q stop")
              , ("S-<XF86AudioLowerVolume>", spawn "mpc -q volume -1")
              , ("S-<XF86AudioRaiseVolume>", spawn "mpc -q volume +1")

              , ("M-<Space>",     sendMessage NextLayout)
              , ("M-S-<Space>",   setLayout $ layoutHook conf)
              , ("M-f",           sendMessage $ Toggle NBFULL)
              , ("M-u",           sendMessage $ Toggle MIRROR)
              , ("M-i",           sendMessage $ Toggle BORDERS)
              , ("M-o",           sendMessage $ Toggle DECORATION)
              , ("M-S-f",         toggleFloat)

              , ("M-j",           windows W.focusUp)
              , ("M-k",           windows W.focusDown)
              , ("M-S-j",         windows W.swapUp)
              , ("M-S-k",         windows W.swapDown)

              , ("M-<Left>",      windows W.focusUp)
              , ("M-<Right>",     windows W.focusDown)
              , ("M-<Up>",        sendMessage $ Go U)
              , ("M-<Down>",      sendMessage $ Go D)
              , ("M-S-<Left>",    windows W.swapUp)
              , ("M-S-<Right>",   windows W.swapDown)
              , ("M-S-<Up>",      sendMessage $ Swap U)
              , ("M-S-<Down>",    sendMessage $ Swap D)

              , ("M-C-<Right>",   moveTo Next NonEmptyWS)
              , ("M-C-<Left>",    moveTo Prev NonEmptyWS)
              , ("M-C-S-<Right>", shiftToNext >> nextWS)
              , ("M-C-S-<Left>",  shiftToPrev >> prevWS)

              , ("M-e",           spawn "xmonad --recompile && xmonad --restart")
              , ("M-S-e",         io (exitWith ExitSuccess))
              ]
              ++
              [ (m ++ show k, windows $ f w)
              | (k, w) <- zip [1..9] $ workspaces conf
              , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
              ]
              ++
              [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
              | (key, sc) <- zip ["[","]","\\"] [0..]
              , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
              ]
  where toggleFloat = withFocused $ \windowId -> do
          floats <- gets (W.floating . windowset)
          if windowId `M.member` floats
            then withFocused $ windows . W.sink
            else float windowId

myKeysAlt = [
  -- program launchers
    -- multimedia keys
    -- prompt
    ("M-r",      shellPrompt myXPConfig)
  , ("M-S-r",    runOrRaisePrompt myXPConfig)

    -- url launcher
  , ("M-s",    safePromptSelection myBrowser)

    -- cycle workspaces
    -- window controls
  ]

-- prompt {{{

myXPConfig = defaultXPConfig { fgColor              = myPromptFGColor
                             , bgColor              = myPromptBGColor
                             , fgHLight             = myPromptBGColor
                             , bgHLight             = myPromptFGColor
                             , promptBorderWidth    = 1
                             , historySize          = 100
                             , font                 = myFont
                             , position             = Top
                             , borderColor          = myBGColor
                             , height               = 18
                             , searchPredicate      = isInfixOf
                             }
-- end prompt }}}
