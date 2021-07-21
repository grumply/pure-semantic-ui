module Semantic.Visibility
  ( module Properties
  , module Tools
  , Passed(..), Calculations(..)
  , Visibility(..), pattern Visibility
  ) where

import Pure hiding (Visibility,offset,context,(#),not,max,Nothing)

import Control.Monad (unless,void,when)
import Data.Coerce
import Data.Foldable (for_,traverse_)
import Data.IORef
import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Context, Context(..)
  , pattern Continuous, Continuous(..)
  , pattern FireOnMount, FireOnMount(..)
  , pattern Offset, Offset(..)
  , pattern OnBottomPassed, OnBottomPassed(..)
  , pattern OnBottomPassedReverse, OnBottomPassedReverse(..)
  , pattern OnBottomVisible, OnBottomVisible(..)
  , pattern OnBottomVisibleReverse, OnBottomVisibleReverse(..)
  , pattern Once, Once(..)
  , pattern OnPassed, OnPassed(..)
  , pattern OnPassing, OnPassing(..)
  , pattern OnPassingReverse, OnPassingReverse(..)
  , pattern OnOffScreen, OnOffScreen(..)
  , pattern OnOnScreen, OnOnScreen(..)
  , pattern OnTopPassed, OnTopPassed(..)
  , pattern OnTopPassedReverse, OnTopPassedReverse(..)
  , pattern OnTopVisible, OnTopVisible(..)
  , pattern OnTopVisibleReverse, OnTopVisibleReverse(..)
  , pattern OnUpdate, OnUpdate(..)
  )

import Data.Function as Tools ((&))

data Passed = PixelsPassed Double | PercentPassed Double
    deriving (Generic,Default,Ord,Eq)

data Visibility = Visibility_
    { as                     :: Features -> [View] -> View
    , features               :: Features
    , children               :: [View]
    , context                :: Maybe JSV
    , continuous             :: Bool
    , fireOnMount            :: Bool
    , offset                 :: (Double,Double)
    , onBottomPassed         :: Maybe (Calculations -> IO ())
    , onBottomPassedReverse  :: Maybe (Calculations -> IO ())
    , onBottomVisible        :: Maybe (Calculations -> IO ())
    , onBottomVisibleReverse :: Maybe (Calculations -> IO ())
    , once                   :: Bool
    , onOffScreen            :: Maybe (Calculations -> IO ())
    , onOnScreen             :: Maybe (Calculations -> IO ())
    , onPassed               :: [(Calculations -> IO (),Passed)]
    , onPassing              :: Maybe (Calculations -> IO ())
    , onPassingReverse       :: Maybe (Calculations -> IO ())
    , onTopPassed            :: Maybe (Calculations -> IO ())
    , onTopPassedReverse     :: Maybe (Calculations -> IO ())
    , onTopVisible           :: Maybe (Calculations -> IO ())
    , onTopVisibleReverse    :: Maybe (Calculations -> IO ())
    , onUpdate               :: Maybe (Calculations -> IO ())
    } deriving (Generic)

instance Default Visibility where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs, context = Just (coerce window), once = True }

pattern Visibility :: Visibility -> Visibility
pattern Visibility v = v

data VisibilityState = VS
    { oldCalculations :: IORef Calculations
    , calculations    :: IORef Calculations
    , verticalOffset  :: IORef Int
    , handlers        :: IORef VisibilityHandlers
    , fired           :: IORef [Txt]
    , ticking         :: IORef Bool
    , ref             :: IORef (Maybe JSV)
    }

data VisibilityHandlers = VH
    { resizeHandler :: IO ()
    , scrollHandler :: IO ()
    } deriving (Generic,Default)

data Direction = Down | Up
    deriving (Generic,Default,Eq,Ord,Show)
instance ToTxt Direction where
    toTxt Up = "up"
    toTxt _  = "down"
instance FromTxt Direction where
    fromTxt "up" = Up
    fromTxt _    = Down

data Calculations = Calculations
    { direction        :: Direction
    , height           :: Double
    , width            :: Double
    , top              :: Double
    , bottom           :: Double
    , percentagePassed :: Double
    , pixelsPassed     :: Double
    , bottomPassed     :: Bool
    , bottomVisible    :: Bool
    , fits             :: Bool
    , passing          :: Bool
    , offScreen        :: Bool
    , onScreen         :: Bool
    , topPassed        :: Bool
    , topVisible       :: Bool
    } deriving (Generic,Default)

instance Pure Visibility where
    view =
        Component $ \self ->
            let
                handleRef (Node n) = do
                    VS {..} <- get self
                    writeIORef ref (Just n)

                execute Nothing _ = return ()
                execute (Just callback) name = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    cs <- readIORef calculations
                    fs <- readIORef fired

                    unless (not continuous && name `elem` fs) $ do
                      callback cs
                      writeIORef fired (name:fs)

                fire callback name value rev = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    oldcs <- readIORef oldCalculations
                    cs    <- readIORef calculations

                    let matchesDirection  =               value cs /= rev
                        executionPossible = continuous || value cs /= value oldcs

                    when (matchesDirection && executionPossible) (execute callback name)

                    unless once $ modifyIORef fired (Prelude.filter (/= name))

                fireOnPassed = do
                    Visibility_  {..} <- ask self
                    VS           {..} <- get self
                    Calculations {..} <- readIORef calculations

                    for_ onPassed $ \(callback,passed) ->
                        let (thresholdReached,name) =
                                case passed of
                                    PixelsPassed pxs  -> (pixelsPassed     >= pxs      ,toTxt pxs)
                                    PercentPassed per -> (percentagePassed >= per / 100,toTxt per)
                        in thresholdReached # execute (Just callback) name

                handleUpdate = do
                    VS {..} <- get self

                    t       <- readIORef ticking

                    unless t $ do
                        writeIORef ticking True
                        void $ addAnimation update

                update = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    writeIORef ticking False

                    writeIORef oldCalculations =<< readIORef calculations
                    writeIORef calculations    =<< compute
                    writeIORef verticalOffset  =<< pageYOffset

                    cs <- readIORef calculations

                    for_ onUpdate ($ cs)

                    fireOnPassed

                    let upd rev (callback,name,selector) = fire callback name selector rev

                    traverse_ (upd True)
                        [ (onBottomPassedReverse,"onBottomPassedReverse",bottomPassed)
                        , (onBottomVisibleReverse,"onBottomVisibleReverse",bottomVisible)
                        , (onPassingReverse,"onPassingReverse",passing)
                        , (onTopPassedReverse,"onTopPassedReverse",topPassed)
                        , (onTopVisibleReverse,"onTopVisibleReverse",topVisible)
                        ]

                    traverse_ (upd False)
                        [ (onBottomPassed,"onBottomPassed",bottomPassed)
                        , (onBottomVisible,"onBottomVisible",bottomVisible)
                        , (onPassing,"onPassing",passing)
                        , (onOffScreen,"onOffScreen",offScreen)
                        , (onOnScreen,"onOnScreen",onScreen)
                        , (onTopPassed,"onTopPassed",topPassed)
                        , (onTopVisible,"onTopVisible",topVisible)
                        ]

                compute = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    Just r <- readIORef ref

                    BR { brBottom = bottom, brHeight = height, brTop = top, brWidth = width } <- boundingRect (Element r)

                    oldPYO <- readIORef verticalOffset
                    newPYO <- pageYOffset
                    ih     <- fromIntegral <$> innerHeight

                    let (topOffset,bottomOffset) = offset

                        direction    = (newPYO > oldPYO) ? Down $ Up
                        topPassed    = top     < topOffset
                        bottomPassed = bottom  < bottomOffset

                        pixelsPassed     = bottomPassed ? 0 $ max (negate top) 0
                        percentagePassed = pixelsPassed / height

                        bottomVisible = bottom >= bottomOffset && bottom <= ih
                        topVisible    = top    >= topOffset    && top    <= ih

                        fits    = topVisible && bottomVisible
                        passing = topPassed  && not bottomPassed

                        onScreen  = (topVisible || topPassed) && not bottomPassed
                        offScreen = not onScreen

                    return Calculations {..}

            in def
                { construct = VS <$> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def

                , mounted = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self
                    for_ context $ \c -> do
                      rh <- onRaw (Node c) "resize" def (\_ _ -> handleUpdate)
                      sh <- onRaw (Node c) "scroll" def (\_ _ -> handleUpdate)
                      writeIORef handlers (VH rh sh)
                    pageYOffset >>= writeIORef verticalOffset
                    when fireOnMount update

                , receive = \newprops oldstate@VS{..} -> do
                    oldprops <- ask self
                    (continuous newprops /= continuous oldprops || once newprops /= once oldprops) #
                        writeIORef fired []
                    return oldstate

                , unmounted = do
                    VS {..} <- get self
                    VH {..} <- readIORef handlers
                    resizeHandler
                    scrollHandler

                , render = \Visibility_ {..} _ ->
                    as (features & WithHost handleRef) children

                }

instance HasProp As Visibility where
    type Prop As Visibility = Features -> [View] -> View
    getProp _ = as
    setProp _ a v = v { as = a }

instance HasFeatures Visibility where
    getFeatures = features
    setFeatures as v = v { features = as }

instance HasChildren Visibility where
    getChildren = children
    setChildren cs v = v { children = cs }

instance HasProp Context Visibility where
    type Prop Context Visibility = Maybe JSV
    getProp _ = context
    setProp _ c v = v { context = c }

instance HasProp Continuous Visibility where
    type Prop Continuous Visibility = Bool
    getProp _ = continuous
    setProp _ c v = v { continuous = c }

instance HasProp FireOnMount Visibility where
    type Prop FireOnMount Visibility = Bool
    getProp _ = fireOnMount
    setProp _ fom v = v { fireOnMount = fom }

instance HasProp OnBottomPassed Visibility where
    type Prop OnBottomPassed Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomPassed
    setProp _ obp v = v { onBottomPassed = obp }

instance HasProp OnBottomPassedReverse Visibility where
    type Prop OnBottomPassedReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomPassedReverse
    setProp _ obpr v = v { onBottomPassedReverse = obpr }

instance HasProp OnBottomVisible Visibility where
    type Prop OnBottomVisible Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomVisible
    setProp _ obv v = v { onBottomVisible = obv }

instance HasProp OnBottomVisibleReverse Visibility where
    type Prop OnBottomVisibleReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomVisibleReverse
    setProp _ obvr v = v { onBottomVisibleReverse = obvr }

instance HasProp Offset Visibility where
    type Prop Offset Visibility = (Double,Double)
    getProp _ = offset
    setProp _ o v = v { offset = o }

instance HasProp Once Visibility where
    type Prop Once Visibility = Bool
    getProp _ = once
    setProp _ o v = v { once = o }

instance HasProp OnPassed Visibility where
    type Prop OnPassed Visibility = [(Calculations -> IO (),Passed)]
    getProp _ = onPassed
    setProp _ op v = v { onPassed = op }

instance HasProp OnPassing Visibility where
    type Prop OnPassing Visibility = Maybe (Calculations -> IO ())
    getProp _ = onPassing
    setProp _ op v = v { onPassing = op }

instance HasProp OnPassingReverse Visibility where
    type Prop OnPassingReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onPassingReverse
    setProp _ opr v = v { onPassingReverse = opr }

instance HasProp OnOffScreen Visibility where
    type Prop OnOffScreen Visibility = Maybe (Calculations -> IO ())
    getProp _ = onOffScreen
    setProp _ oos v = v { onOffScreen = oos }

instance HasProp OnOnScreen Visibility where
    type Prop OnOnScreen Visibility = Maybe (Calculations -> IO ())
    getProp _ = onOnScreen
    setProp _ oos v = v { onOnScreen = oos }

instance HasProp OnTopPassed Visibility where
    type Prop OnTopPassed Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopPassed
    setProp _ otp v = v { onTopPassed = otp }

instance HasProp OnTopPassedReverse Visibility where
    type Prop OnTopPassedReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopPassedReverse
    setProp _ otpr v = v { onTopPassedReverse = otpr }

instance HasProp OnTopVisible Visibility where
    type Prop OnTopVisible Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopVisible
    setProp _ otv v = v { onTopVisible = otv }

instance HasProp OnTopVisibleReverse Visibility where
    type Prop OnTopVisibleReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopVisibleReverse
    setProp _ otvr v = v { onTopVisibleReverse = otvr }

instance HasProp OnUpdate Visibility where
    type Prop OnUpdate Visibility = Maybe (Calculations -> IO ())
    getProp _ = onUpdate
    setProp _ ou v = v { onUpdate = ou }
