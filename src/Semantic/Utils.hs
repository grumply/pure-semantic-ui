{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Semantic.Utils (module Semantic.Utils, module Export) where

import Pure.Data
import Pure.View hiding (one,two,Width)

import Pure.Lifted

import qualified Pure.Data.Txt as Txt

import Data.Function as Export

useKeyOrValueAndKey val key =
  case val of
    Just "" -> key
    Just v  -> v <<>> key
    _       -> nil

multiProp val key
    | isNil val = nil
    | otherwise =
          Txt.unwords
        . map (\w -> Txt.replace "-" " " w <<>> key)
        . Txt.words
        . Txt.replace " vertically" "-vertically"
        . Txt.replace "lare screen" "large-screen"
        . Txt.unwords
        $ val

widthProp :: Width -> Txt -> Bool -> Txt
widthProp val widthClass canEqual
    | isNil val = ""
    | canEqual && val == equal = "equal width"
    | otherwise = toTxt val <>> widthClass

newtype Width = Width Txt deriving (Eq)
instance Default Width where def = Width ""
instance Cond Width where 
    nil = Width ""
    isNil (Width "") = True
    isNil _          = False
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

only :: [a] -> a
only [a] = a
only _ = error "expected a singular list element"

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

extractInputAttrs = foldr go ([],[])
    where
        go x ~(inputAttrs,otherAttrs) =
            let isInputAttr =
                    case x of
                        On ev _ f -> ev `elem` 
                            ["keydown","keypress","keyup","focus","blur","change","input","click","contextmenu"
                            ,"drag","dragend","dragenter","dragexit","dragleave","dragover","dragstart","drop"
                            ,"mousedown","mouseenter","mouseleave","mousemove","mouseout","mouseover","mouseup"
                            ,"select","touchcancel","touchend","touchmove","touchstart"]
                        Prop p v -> p `elem`
                            ["autocapitalize","autocomplete","autocorrect","autofocus","checked","disabled","form"
                            ,"id","list","max","maxlength","min","minlength","multiple","name","pattern","placeholder"
                            ,"readonly","required","step","type","value"]
                        _ -> False
            in if isInputAttr 
                   then (x:inputAttrs,otherAttrs)
                   else (inputAttrs,x:otherAttrs)

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.contains($2)" contains_js :: JSV -> JSV -> IO Bool
#endif

contains :: MonadIO c => JSV -> JSV -> c Bool
contains node target = 
#ifdef __GHCJS__
    liftIO $ contains_js node target
#else 
    return True -- hmm?
#endif

cloneWithProps :: forall ms. View ms -> [Feature ms] -> View ms
cloneWithProps v fs =
    case v of
        ComponentView {..} -> ComponentView { componentView = cloneComponent . componentView, .. }
        SomeView sv        -> cloneWithProps (render sv) fs
        NullView {}        -> v
        TextView {}        -> v
        _                  -> v { features = features v ++ fs }
    where
        cloneComponent :: forall props state. Comp ms props state -> Comp ms props state
        cloneComponent Comp {..} = Comp { renderer = \p s -> cloneWithProps (renderer p s) fs,  .. }

#ifdef __GHCJS__
foreign import javascript unsafe
    "document.body" body_js :: JSV
#endif

body :: JSV
body =
#ifdef __GHCJS__
    body_js
#else
    ()
#endif
