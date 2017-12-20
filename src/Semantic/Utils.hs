module Semantic.Utils where

import Pure.Data

data TextAlignment = AlignedLeft | AlignedCenter | AlignedRight | AlignedJustified deriving (Eq,Ord,Generic,Default)

textAlignClass (Just ta) = 
  case ta of
    AlignedLeft -> "left aligned"
    AlignedCenter -> "center aligned"
    AlignedRight -> "right aligned"
    AlignedJustified -> "justified"
