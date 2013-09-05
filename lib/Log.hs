module Log where

import System.Taffybar.XMonadLog
import XMonad.Hooks.DynamicLog
import XMonad.Util.Scratchpad

scratchpadFilterOutWorkspacePP pp =
  pp { ppSort = fmap (. scratchpadFilterOutWorkspace) (ppSort pp)
     }

myLogHook client = dbusLogWithPP client $
                   scratchpadFilterOutWorkspacePP
                   taffybarPP { ppHiddenNoWindows = taffybarEscape . const ""
                              }
