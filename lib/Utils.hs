module Utils where

import Data.List
import XMonad
import XMonad.StackSet             as W
import XMonad.Util.NamedScratchpad

-- Better matching using `isInfixOf`
-- | @q =* x@. if @x@ `isInfixOf` the result of @q@, return 'True'.
(=*) :: Eq a => Query [a] -> [a] -> Query Bool
q =* x = fmap (x `isInfixOf`) q

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "tmux" "urxvtc -T tmux -e tmux attach" (title =* "tmux")
                (customFloating $ W.RationalRect 0.02 0.05 0.96 0.9)
              , NS "mutt" "urxvtc -T mutt -e mutt" (title =* "mutt")
                (customFloating $ W.RationalRect 0.02 0.05 0.96 0.9)
              , NS "ncmpcpp" "urxvtc -T ncmpcpp -e ncmpcpp" (title =* "ncmpcpp")
                (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
              , NS "alsamixer" "urxvtc -T alsamixer -e alsamixer" (title =* "alsamixer")
                (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
              , NS "htop" "urxvtc -T htop -e sudo -n htop" (title =* "htop")
                (customFloating $ W.RationalRect 0.02 0.05 0.96 0.9)
              ]
