{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Semantic.Utils (module Semantic.Utils, module Export) where

import Pure.Data hiding (lookup)
import Pure.View hiding (one,two,lookup,reverse,Width)

import Pure.Lifted hiding (lookup)
import Pure.Data.JSV

import qualified Pure.Data.Txt as Txt

import Data.Function as Export
import Data.Maybe

import Control.Monad.ST
import Data.STRef

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

extractInputAttrs :: Foldable f => f (Feature ms) -> ([Feature ms],[Feature ms])
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
    "$1 !== $2" not_equal_js :: JSV -> JSV -> IO Bool
#endif

unequalTargets :: JSV -> JSV -> IO Bool
unequalTargets x y =
#ifdef __GHCJS__
    not_equal_js x y
#else
    return True
#endif

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

clone :: forall ms. ([Feature ms] -> [Feature ms]) -> View ms -> View ms
clone f v =
    case v of
        ComponentView {..} -> ComponentView { componentView = cloneComponent . componentView, .. }
        SomeView sv        -> clone f (render sv)
        NullView {}        -> v
        TextView {}        -> v
        _                  -> v { features = f (features v) }
    where
        cloneComponent :: forall props state. Comp ms props state -> Comp ms props state
        cloneComponent Comp {..} = Comp { renderer = \p s -> clone f (renderer p s), .. }
        
updateClasses :: forall ms. ([Txt] -> [Txt]) -> View ms -> View ms
updateClasses f = clone (go False)
    where
        go False [] = [ ClassList (f []) ]
        go True  [] = []
        go _ (ClassList cs : fs) = ClassList (f cs) : go True fs
        go b (f : fs) = f : go b fs

updateStyles :: forall ms. ([(Txt,Txt)] -> [(Txt,Txt)]) -> View ms -> View ms
updateStyles f = clone (go False)
    where
        go False [] = [ StyleList (f []) ]
        go True  [] = []
        go _ (StyleList ss : fs) = StyleList (f ss) : go True fs
        go b (f : fs) = f : go b fs

updateStylesAndClasses :: forall ms. ([(Txt,Txt)] -> [(Txt,Txt)]) -> ([Txt] -> [Txt]) -> View ms -> View ms
updateStylesAndClasses s c = clone (go False False) 
    where
        go False False []        = [ StyleList (s []), ClassList (c []) ]
        go False True  []        = [ StyleList (s []) ]
        go True  False []        = [ ClassList (c []) ]
        go _ y (StyleList ss : fs) = StyleList (s ss) : go True y fs
        go x _ (ClassList cs : fs) = ClassList (c cs) : go x True fs
        go x y (f : fs)            = f : go x y fs

cloneWithProps :: forall ms. View ms -> [Feature ms] -> View ms
cloneWithProps v fs = clone (++ fs) v

directionalTransitions = 
    [ "scale"
    , "fade", "fade up", "fade down", "fade left", "fade right"
    , "horizontal flip", "vertical flip"
    , "drop"
    , "fly left", "fly right", "fly up", "fly down"
    , "swing left", "swing right", "swing up", "swing down"
    , "browse", "browse right"
    , "slide down", "slide up", "slide right"
    ]

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

#ifdef __GHCJS__
foreign import javascript unsafe
    "window.pageYOffset" pageYOffset_js ::  IO Int
#endif

pageYOffset :: MonadIO c => c Int
pageYOffset = 
#ifdef __GHCJS__
    liftIO pageYOffset_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$r = $1.getBoundingClientRect()" bounding_client_rect_js :: Element -> IO Obj
#endif

-- (bottom,height,top,width)
boundingRect :: MonadIO c => Element -> c (Double,Double,Double,Double)
boundingRect node = do
#ifdef __GHCJS__
  rect <- liftIO $ bounding_client_rect_js node
  return $ fromJust $ parse rect $ \o ->
      (,,,) <$> (o .: "bottom") 
            <*> (o .: "height") 
            <*> (o .: "top") 
            <*> (o .: "width")
#else
    return (0,0,0,0)
#endif

#ifdef __GHCJS__
foreign import javascript unsafe 
    "window.innerHeight" innerHeight_js :: IO Int
#endif

innerHeight :: MonadIO c => c Int
innerHeight =
#ifdef __GHCJS__
    liftIO innerHeight_js
#else
    return 0
#endif

-- This is hideous; functionalize.
{-# INLINE mergeMappings #-}
mergeMappings :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeMappings prev next = runST $ do
    childMapping    <- newSTRef []
    nextKeysPending <- newSTRef []
    pendingKeys     <- newSTRef []

    let swap ref ys = do
            xs <- readSTRef ref 
            writeSTRef ref ys 
            return xs

    for_ prev $ \(prevKey,_) -> do
        case lookup prevKey next of
            Just _ -> do
                pks <- swap pendingKeys []
                modifySTRef nextKeysPending ((prevKey,reverse pks):)

            Nothing -> 
                modifySTRef pendingKeys (prevKey:)

    let value k = fromJust (lookup k next <|> lookup k prev)
        addChildMapping k = modifySTRef childMapping ((:) (k,value k))

    nkps <- readSTRef nextKeysPending
    for_ next $ \(nextKey,_) -> do
        for_ (lookup nextKey nkps) 
            (traverse_ addChildMapping)
        addChildMapping nextKey

    pks <- readSTRef pendingKeys
    for_ (reverse pks) addChildMapping

    reverse <$> readSTRef childMapping

-- Direct transcription. I'm sure there is a much simpler representation,
-- but I don't have the time to walk through the algorithm.
{-# INLINE mergeMappings' #-}
mergeMappings' :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeMappings' prev next = reverse result
    where
        value k = lookup k next <|> lookup k prev

        result = foldr mergePending childMapping leftovers 
          where
            mergePending k@(value -> Just v) = ((k,v):)
            mergePending _ = id

        childMapping = foldl' mergeNext [] next 
          where
            addChildMapping k = maybe id ((:) . (k,)) (value k)
            mergeNext childMapping (k,v) = addChildMapping k $
                case lookup k pending of
                    Nothing -> childMapping
                    Just ks -> foldl' (flip addChildMapping) childMapping ks

        (pending,leftovers) = foldl' mergePrev ([],[]) prev 
          where
            mergePrev (nkp,pks) (k@(flip lookup next -> Nothing),_) = (nkp,k:pks)
            mergePrev (nkp,[]) (k,_)  = (nkp,[])
            mergePrev (nkp,pks) (k,_) = ((k,reverse pks):nkp,[])

#ifdef __GHCJS__
foreign import javascript unsafe
    "document.body.classList.add($1)" addBodyClass_js :: Txt -> IO ()
#endif

addBodyClass :: MonadIO c => Txt -> c ()
addBodyClass c =
#ifdef __GHCJS__
    liftIO $ addBodyClass_js c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "document.body.classList.remove($1)" removeBodyClass_js :: Txt -> IO ()
#endif

removeBodyClass :: MonadIO c => Txt -> c ()
removeBodyClass c =
#ifdef __GHCJS__
    liftIO $ removeBodyClass_js c
#else
    return ()
#endif