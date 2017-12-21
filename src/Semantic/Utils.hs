module Semantic.Utils (module Semantic.Utils, module Export) where

import Pure.Data
import Pure.View

import Data.Function as Export

data TextAlignment = AlignedLeft | AlignedCenter | AlignedRight | AlignedJustified deriving (Eq,Ord,Generic,Default)

textAlignClass (Just ta) = 
  case ta of
    AlignedLeft -> "left aligned"
    AlignedCenter -> "center aligned"
    AlignedRight -> "right aligned"
    AlignedJustified -> "justified"

useKeyOrValueAndKey val key =
  maybe key (<<>> key) val

useValueAndKey val key =
  val ? (val <<>> key) $ key

oneEq :: Eq a => a -> a -> a -> Maybe a
oneEq l r x = if l == x then Just l else if r == x then Just r else Nothing

-- if x is nil then y else nil
(#!) :: (Cond x, Default a) => x -> a -> a
(#!) x y = (isNil x) # y

-- if x is (Just non-nil) then y else nil
(#?) :: (Cond x, Default a, Cond a) => Maybe x -> a -> a
(#?) x y = may (# y) x

-- if x is (Just nil) then y else nil
(#!?) :: (Cond x, Default a, Cond a) => Maybe x -> a -> a
(#!?) x y = may (#! y) x

