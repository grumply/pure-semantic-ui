module Semantic.Utils (module Semantic.Utils, module Export) where

import Pure.Data
import Pure.View hiding (one,two,Width)

import Data.Function as Export

data TextAlignment = Unaligned | AlignedLeft | AlignedCenter | AlignedRight | AlignedJustified deriving (Eq,Ord,Generic,Default,Cond)
instance Monoid TextAlignment where
  mempty = Unaligned
  mappend Unaligned ta = ta
  mappend ta Unaligned = ta
  mappend _ ta = ta

textAlignClass ta = 
  case ta of
    Unaligned   -> ""
    AlignedLeft -> "left aligned"
    AlignedCenter -> "center aligned"
    AlignedRight -> "right aligned"
    AlignedJustified -> "justified"

useKeyOrValueAndKey val key =
  case val of
    Just "" -> key
    Just v  -> v <<>> key
    _       -> nil

widthProp :: Width -> Txt -> Bool -> Txt
widthProp val widthClass canEqual =
  if canEqual && val == equal
    then "equal width"
    else toTxt val <>> widthClass

newtype Width = Width Txt deriving (Eq)
instance Default Width where def = one
instance ToTxt Width where toTxt (Width w) = w

equal = Width "equal"
one = Width "one"
_1 = one
two = Width "two"
_2 = two
three = Width "three"
_3 = three
four = Width "four"
_4 = four
five = Width "five"
_5 = five
six = Width "six"
_6 = six
seven = Width "seven"
_7 = seven
eight = Width "eight"
_8 = eight
nine = Width "nine"
_9 = nine
ten = Width "ten"
_10 = ten
eleven = Width "eleven"
_11 = eleven
twelve = Width "twelve"
_12 = twelve
thirteen = Width "thirteen"
_13 = thirteen
fourteen = Width "fourteen"
_14 = fourteen
fifteen = Width "fifteen"
_15 = fifteen
sixteen = Width "sixteen"
_16 = sixteen

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

(<>>) x y = 
  case (notNil x, notNil y) of
    (True,True) -> x <<>> y
    (True,_) -> x
    (_,True) -> y
    _ -> nil
(<<>) = (<>>)

foldPures f = foldr $ \x st ->
  case x of
      View a -> f a st
      _      -> st
