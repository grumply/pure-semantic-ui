module Semantic.Visibility
  ( module Properties
  , module Tools
  , Passed(..), Calculations(..)
  , Visibility(..), pattern Visibility
  ) where

import Data.Coerce
import Data.IORef
import GHC.Generics as G
import Pure.View hiding (offset,Visibility,Offset)
import Pure.Lifted hiding (Offset)
import Pure.DOM (addAnimation)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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
import Pure.Data.Default as Tools

data Passed = PixelsPassed Double | PercentPassed Double
    deriving (Generic,Default,Ord,Eq)

data Visibility ms = Visibility_
    { as                     :: [Feature ms] -> [View ms] -> View ms
    , attributes             :: [Feature ms]
    , children               :: [View ms]
    , classes                :: [Txt]
    , context                :: Maybe JSV
    , continuous             :: Bool
    , fireOnMount            :: Bool
    , offset                 :: (Double,Double)
    , onBottomPassed         :: Maybe (Calculations -> Ef ms IO ())
    , onBottomPassedReverse  :: Maybe (Calculations -> Ef ms IO ())
    , onBottomVisible        :: Maybe (Calculations -> Ef ms IO ())
    , onBottomVisibleReverse :: Maybe (Calculations -> Ef ms IO ())
    , once                   :: Bool
    , onOffScreen            :: Maybe (Calculations -> Ef ms IO ())
    , onOnScreen             :: Maybe (Calculations -> Ef ms IO ())
    , onPassed               :: [(Calculations -> Ef ms IO (),Passed)]
    , onPassing              :: Maybe (Calculations -> Ef ms IO ())
    , onPassingReverse       :: Maybe (Calculations -> Ef ms IO ())
    , onTopPassed            :: Maybe (Calculations -> Ef ms IO ())
    , onTopPassedReverse     :: Maybe (Calculations -> Ef ms IO ())
    , onTopVisible           :: Maybe (Calculations -> Ef ms IO ())
    , onTopVisibleReverse    :: Maybe (Calculations -> Ef ms IO ())
    , onUpdate               :: Maybe (Calculations -> Ef ms IO ())
    } deriving (Generic)

instance Default (Visibility ms) where
    def = (G.to gdef) { as = Div, context = Just (coerce window), once = True }

pattern Visibility :: Visibility ms -> View ms
pattern Visibility v = View v

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

instance Pure Visibility ms where
    render v =
        Component "Semantic.Behaviors.Visibility" v $ \self ->
            let
                handleRef (Node n) = do
                    VS {..} <- getState self
                    writeIORef ref (Just n)
                    return Nothing

                execute Nothing _ = return ()
                execute (Just callback) name = do
                    Visibility_ {..} <- getProps self
                    VS          {..} <- getState self

                    cs <- readIORef calculations
                    fs <- readIORef fired

                    unless (not continuous && name `elem` fs) $ do
                      parent self (callback cs)
                      writeIORef fired (name:fs)

                fire callback name value rev = do
                    Visibility_ {..} <- getProps self
                    VS          {..} <- getState self

                    oldcs <- readIORef oldCalculations
                    cs    <- readIORef calculations

                    let matchesDirection  =               value cs /= rev
                        executionPossible = continuous || value cs /= value oldcs

                    when (matchesDirection && executionPossible) (execute callback name)

                    unless once $ modifyIORef fired (filter (/= name))

                fireOnPassed = do
                    Visibility_  {..} <- getProps self
                    VS           {..} <- getState self
                    Calculations {..} <- readIORef calculations

                    for_ onPassed $ \(callback,passed) ->
                        let (thresholdReached,name) =
                                case passed of
                                    PixelsPassed pxs  -> (pixelsPassed     >= pxs      ,toTxt pxs)
                                    PercentPassed per -> (percentagePassed >= per / 100,toTxt per)
                        in thresholdReached # execute (Just callback) name

                handleUpdate = do
                    VS {..} <- getState self

                    t       <- readIORef ticking

                    unless t $ do
                        writeIORef ticking True
                        void $ addAnimation update

                update = do
                    Visibility_ {..} <- getProps self
                    VS          {..} <- getState self

                    writeIORef ticking False

                    writeIORef oldCalculations =<< readIORef calculations
                    writeIORef calculations    =<< compute
                    writeIORef verticalOffset  =<< pageYOffset

                    cs <- readIORef calculations

                    for_ onUpdate $ \ou ->
                        parent self (ou cs)

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
                    Visibility_ {..} <- getProps self
                    VS          {..} <- getState self

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
                    Visibility_ {..} <- getProps self
                    VS          {..} <- getState self
                    for_ context $ \c -> do
                      rh <- onRaw (Node c) "resize" def (\_ _ -> handleUpdate)
                      sh <- onRaw (Node c) "scroll" def (\_ _ -> handleUpdate)
                      writeIORef handlers (VH rh sh)
                    pageYOffset >>= writeIORef verticalOffset
                    when fireOnMount update

                , receiveProps = \newprops oldstate@VS{..} -> do
                    oldprops <- getProps self
                    (continuous newprops /= continuous oldprops || once newprops /= once oldprops) #
                        writeIORef fired []
                    return oldstate

                , unmount = do
                    VS {..} <- getState self
                    VH {..} <- readIORef handlers
                    resizeHandler
                    scrollHandler

                , renderer = \Visibility_ {..} _ ->
                    as
                        ( mergeClasses $ ClassList classes
                        : HostRef handleRef
                        : attributes
                        )
                        children

                }

instance HasProp As (Visibility ms) where
    type Prop As (Visibility ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a v = v { as = a }

instance HasProp Attributes (Visibility ms) where
    type Prop Attributes (Visibility ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as v = v { attributes = as }

instance HasProp Children (Visibility ms) where
    type Prop Children (Visibility ms) = [View ms]
    getProp _ = children
    setProp _ cs v = v { children = cs }

instance HasProp Classes (Visibility ms) where
    type Prop Classes (Visibility ms) = [Txt]
    getProp _ = classes
    setProp _ cs v = v { classes = cs }

instance HasProp Context (Visibility ms) where
    type Prop Context (Visibility ms) = Maybe JSV
    getProp _ = context
    setProp _ c v = v { context = c }

instance HasProp Continuous (Visibility ms) where
    type Prop Continuous (Visibility ms) = Bool
    getProp _ = continuous
    setProp _ c v = v { continuous = c }

instance HasProp FireOnMount (Visibility ms) where
    type Prop FireOnMount (Visibility ms) = Bool
    getProp _ = fireOnMount
    setProp _ fom v = v { fireOnMount = fom }

instance HasProp OnBottomPassed (Visibility ms) where
    type Prop OnBottomPassed (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onBottomPassed
    setProp _ obp v = v { onBottomPassed = obp }

instance HasProp OnBottomPassedReverse (Visibility ms) where
    type Prop OnBottomPassedReverse (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onBottomPassedReverse
    setProp _ obpr v = v { onBottomPassedReverse = obpr }

instance HasProp OnBottomVisible (Visibility ms) where
    type Prop OnBottomVisible (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onBottomVisible
    setProp _ obv v = v { onBottomVisible = obv }

instance HasProp OnBottomVisibleReverse (Visibility ms) where
    type Prop OnBottomVisibleReverse (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onBottomVisibleReverse
    setProp _ obvr v = v { onBottomVisibleReverse = obvr }

instance HasProp Offset (Visibility ms) where
    type Prop Offset (Visibility ms) = (Double,Double)
    getProp _ = offset
    setProp _ o v = v { offset = o }

instance HasProp Once (Visibility ms) where
    type Prop Once (Visibility ms) = Bool
    getProp _ = once
    setProp _ o v = v { once = o }

instance HasProp OnPassed (Visibility ms) where
    type Prop OnPassed (Visibility ms) = [(Calculations -> Ef ms IO (),Passed)]
    getProp _ = onPassed
    setProp _ op v = v { onPassed = op }

instance HasProp OnPassing (Visibility ms) where
    type Prop OnPassing (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onPassing
    setProp _ op v = v { onPassing = op }

instance HasProp OnPassingReverse (Visibility ms) where
    type Prop OnPassingReverse (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onPassingReverse
    setProp _ opr v = v { onPassingReverse = opr }

instance HasProp OnOffScreen (Visibility ms) where
    type Prop OnOffScreen (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onOffScreen
    setProp _ oos v = v { onOffScreen = oos }

instance HasProp OnOnScreen (Visibility ms) where
    type Prop OnOnScreen (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onOnScreen
    setProp _ oos v = v { onOnScreen = oos }

instance HasProp OnTopPassed (Visibility ms) where
    type Prop OnTopPassed (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onTopPassed
    setProp _ otp v = v { onTopPassed = otp }

instance HasProp OnTopPassedReverse (Visibility ms) where
    type Prop OnTopPassedReverse (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onTopPassedReverse
    setProp _ otpr v = v { onTopPassedReverse = otpr }

instance HasProp OnTopVisible (Visibility ms) where
    type Prop OnTopVisible (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onTopVisible
    setProp _ otv v = v { onTopVisible = otv }

instance HasProp OnTopVisibleReverse (Visibility ms) where
    type Prop OnTopVisibleReverse (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onTopVisibleReverse
    setProp _ otvr v = v { onTopVisibleReverse = otvr }

instance HasProp OnUpdate (Visibility ms) where
    type Prop OnUpdate (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getProp _ = onUpdate
    setProp _ ou v = v { onUpdate = ou }
