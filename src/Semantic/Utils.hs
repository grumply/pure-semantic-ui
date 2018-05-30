{-# LANGUAGE CPP #-}
module Semantic.Utils where

-- from base
import Control.Applicative (Alternative(..))
import Control.Monad.ST (ST,runST)
import Data.Foldable (for_,traverse_)
import Data.List (partition,foldl')
import Data.Maybe (fromMaybe,fromJust)
import Data.Monoid ((<>))
import Data.STRef (newSTRef,readSTRef,writeSTRef,modifySTRef')
import GHC.Generics (Generic)

-- from pure-default
import Pure.Data.Default ((?),Default(..))

-- from pure-lifted
import Pure.Data.Lifted hiding (lookup)

-- from pure-txt
import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Pure.Data.Txt as Txt

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Events as Ev

(<<>>) :: Txt -> Txt -> Txt
(<<>>) x y = x `Txt.append` " " `Txt.append` y

(#) :: Default a => Bool -> a -> a
(#) b t = b ? t $ def

useKeyOrValueAndKey val key =
  case val of
    Just "" -> key
    Just v  -> v <<>> key
    _       -> def

multiProp :: [Txt] -> Txt -> Txt
multiProp val key
    | Prelude.null val = def
    | otherwise =
          Txt.unwords
        . fmap (\w -> Txt.replace "-" " " w <<>> key)
        . Txt.words
        . Txt.replace " vertically" "-vertically"
        . Txt.replace "lare screen" "large-screen"
        . Txt.unwords
        $ val

widthProp :: Txt -> Txt -> Bool -> Txt
widthProp val widthClass canEqual
    | Txt.null val = def
    | canEqual && val == "equal" = "equal width"
    | otherwise = toTxt val <>> widthClass

only :: [a] -> a
only [a] = a
only _ = error "expected a singular list element"

oneEq :: Eq a => a -> a -> a -> Maybe a
oneEq l r x = if l == x then Just l else if r == x then Just r else Nothing

(<>>) x y =
  case (x /= "", y /= "") of
    (True,True) -> x <<>> y
    (True,_) -> x
    (_,True) -> y
    _ -> ""
(<<>) = (<>>)

foldPures f = foldr $ \x st ->
  case x of
      View a -> f a st
      _      -> st

extractInputListeners :: [Listener] -> ([Listener],[Listener])
extractInputListeners = partition (\(Ev.On ev _) -> ev `elem` inputEvents)
  where
    inputEvents =
      ["keydown","keypress","keyup","focus","blur","change","input","click","contextmenu"
      ,"drag","dragend","dragenter","dragexit","dragleave","dragover","dragstart","drop"
      ,"mousedown","mouseenter","mouseleave","mousemove","mouseout","mouseover","mouseup"
      ,"select","touchcancel","touchend","touchmove","touchstart"
      ]

extractInputAttributes :: [(Txt,Txt)] -> ([(Txt,Txt)],[(Txt,Txt)])
extractInputAttributes = partition (\(k,_) -> k `elem` inputAttrs)
  where
    inputAttrs =
      ["autocapitalize","autocomplete","autocorrect","autofocus"
      ,"checked","disabled","form","id","list","max","maxlength"
      ,"min","minlength","multiple","name","pattern","placeholder"
      ,"readonly","required","step","type","value"
      ]

extractInputProperties :: [(Txt,Txt)] -> ([(Txt,Txt)],[(Txt,Txt)])
extractInputProperties = partition (\(k,_) -> k `elem` inputProps)
  where
    inputProps =
      ["autocapitalize","autocomplete","autocorrect","autofocus"
      ,"checked","disabled","form","id","list","max","maxlength"
      ,"min","minlength","multiple","name","pattern","placeholder"
      ,"readonly","required","step","type","value"
      ]

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

contains :: JSV -> JSV -> IO Bool
contains node target =
#ifdef __GHCJS__
    contains_js node target
#else
    return True -- hmm?
#endif

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
    "$r = window.pageYOffset" pageYOffset_js :: IO Int
#endif

pageYOffset :: IO Int
pageYOffset =
#ifdef __GHCJS__
    pageYOffset_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = window.pageXOffset" pageXOffset_js :: IO Int
#endif

pageXOffset :: IO Int
pageXOffset =
#ifdef __GHCJS__
    pageXOffset_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = document.documentElement.clientWidth" clientWidth_js :: IO Int
#endif

clientWidth :: IO Int
clientWidth =
#ifdef __GHCJS__
    clientWidth_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = document.documentElement.clientHeight" clientHeight_js :: IO Int
#endif

clientHeight :: IO Int
clientHeight =
#ifdef __GHCJS__
    clientHeight_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe "$r = $1.getBoundingClientRect()" bounding_client_rect_js :: Element -> IO JSV
#endif

data BoundingRect = BR
    { brLeft :: Double
    , brTop :: Double
    , brRight :: Double
    , brBottom :: Double
    , brWidth :: Double
    , brHeight :: Double
    } deriving (Eq)

instance Default BoundingRect where def = BR 0 0 0 0 0 0

boundingRect :: Element -> IO BoundingRect
boundingRect node = do
#ifdef __GHCJS__
  o <- bounding_client_rect_js node
  return $ fromMaybe (error "Semantic.Utils.boundingRect: fromMaybe got Nothing") $ do
    brLeft   <- o .# "left"
    brTop    <- o .# "top"
    brRight  <- o .# "right"
    brBottom <- o .# "bottom"
    brWidth  <- o .# "width"
    brHeight <- o .# "height"
    return BR {..}
#else
    return $ BR 0 0 0 0 0 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = window.getComputedStyle($1)" computed_styles_js :: Element -> IO JSV
#endif

computedStyles :: Element -> IO JSV
computedStyles node = do
#ifdef __GHCJS__
    computed_styles_js node
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "window.innerHeight" innerHeight_js :: IO Int
#endif

innerHeight :: IO Int
innerHeight =
#ifdef __GHCJS__
    innerHeight_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = window.innerWidth" innerWidth_js :: IO Int
#endif

innerWidth :: IO Int
innerWidth =
#ifdef __GHCJS__
    innerWidth_js
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
                modifySTRef' nextKeysPending ((prevKey,Prelude.reverse pks):)

            Nothing ->
                modifySTRef' pendingKeys (prevKey:)

    let value k = fromJust (lookup k next <|> lookup k prev)
        addChildMapping k = modifySTRef' childMapping ((:) (k,value k))

    nkps <- readSTRef nextKeysPending
    for_ next $ \(nextKey,_) -> do
        for_ (lookup nextKey nkps)
            (traverse_ addChildMapping)
        addChildMapping nextKey

    pks <- readSTRef pendingKeys
    for_ (Prelude.reverse pks) addChildMapping

    Prelude.reverse <$> readSTRef childMapping

-- Direct transcription. I'm sure there is a much simpler representation,
-- but I don't have the time to walk through the algorithm.
{-# INLINE mergeMappings' #-}
mergeMappings' :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeMappings' prev next = Prelude.reverse result
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
            mergePrev (nkp,pks) (k,_) = ((k,Prelude.reverse pks):nkp,[])

#ifdef __GHCJS__
foreign import javascript unsafe
    "document.body.classList.add($1)" addBodyClass_js :: Txt -> IO ()
#endif

addBodyClass :: Txt -> IO ()
addBodyClass c =
#ifdef __GHCJS__
    addBodyClass_js c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "document.body.classList.remove($1)" removeBodyClass_js :: Txt -> IO ()
#endif

removeBodyClass :: Txt -> IO ()
removeBodyClass c =
#ifdef __GHCJS__
    removeBodyClass_js c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.classList.add($2)" addClass_js :: JSV -> Txt -> IO ()
#endif

addClass :: JSV -> Txt -> IO ()
addClass n c =
#ifdef __GHCJS__
    addClass_js n c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$1.classList.remove($2)" removeClass_js :: JSV -> Txt -> IO ()
#endif

removeClass :: JSV -> Txt -> IO ()
removeClass n c =
#ifdef __GHCJS__
    removeClass_js n c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = $1.style[$2]" get_style_js :: Element -> Txt -> IO Txt
#endif

getStyle :: Element -> Txt -> IO (Maybe Txt)
getStyle e s = do
#ifdef __GHCJS__
    s <- get_style_js e s
    return (isNull s ? Nothing $ Just s)
#else
    return mempty
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = $1.scrollHeight" scrollHeight_js :: Element -> IO Int
#endif

scrollHeight :: Element -> IO Int
scrollHeight e = do
#ifdef __GHCJS__
    scrollHeight_js e
#else
    return 0
#endif
