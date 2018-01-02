module Semantic.Behaviors.Visibility where

import Data.Coerce
import Data.IORef
import GHC.Generics as G
import Pure.View hiding (offset)
import Pure.Lifted
import Pure.DOM (addAnimation)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Context
import Semantic.Properties.Continuous
import Semantic.Properties.FireOnMount
import Semantic.Properties.Offset
import Semantic.Properties.OnBottomPassed
import Semantic.Properties.OnBottomPassedReverse
import Semantic.Properties.OnBottomVisible
import Semantic.Properties.OnBottomVisibleReverse
import Semantic.Properties.Once
import Semantic.Properties.OnPassed
import Semantic.Properties.OnPassing
import Semantic.Properties.OnPassingReverse
import Semantic.Properties.OnOffScreen
import Semantic.Properties.OnOnScreen
import Semantic.Properties.OnTopPassed
import Semantic.Properties.OnTopPassedReverse
import Semantic.Properties.OnTopVisible
import Semantic.Properties.OnTopVisibleReverse
import Semantic.Properties.OnUpdate

data Passed = PixelsPassed Double | PercentPassed Double
    deriving (Generic,Default,Ord,Eq)

data Visibility ms = Visibility_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , context :: Maybe JSV
    , continuous :: Bool
    , fireOnMount :: Bool
    , offset :: (Double,Double)
    , onBottomPassed :: Maybe (Calculations -> Ef ms IO ())
    , onBottomPassedReverse :: Maybe (Calculations -> Ef ms IO ())
    , onBottomVisible :: Maybe (Calculations -> Ef ms IO ())
    , onBottomVisibleReverse :: Maybe (Calculations -> Ef ms IO ())
    , once :: Bool
    , onOffScreen :: Maybe (Calculations -> Ef ms IO ())
    , onOnScreen :: Maybe (Calculations -> Ef ms IO ())
    , onPassed :: [(Calculations -> Ef ms IO (),Passed)]
    , onPassing :: Maybe (Calculations -> Ef ms IO ())
    , onPassingReverse :: Maybe (Calculations -> Ef ms IO ())
    , onTopPassed :: Maybe (Calculations -> Ef ms IO ())
    , onTopPassedReverse :: Maybe (Calculations -> Ef ms IO ())
    , onTopVisible :: Maybe (Calculations -> Ef ms IO ())
    , onTopVisibleReverse :: Maybe (Calculations -> Ef ms IO ())
    , onUpdate :: Maybe (Calculations -> Ef ms IO ())
    } deriving (Generic)

instance Default (Visibility ms) where
    def = (G.to gdef) { as = Div, context = Just (coerce window), once = True }

pattern Visibility :: Typeable ms => Visibility ms -> View ms
pattern Visibility v = View v

data VisibilityState = VS
    { oldCalculations :: IORef Calculations
    , calculations :: IORef Calculations
    , verticalOffset :: IORef Int
    , handlers :: IORef VisibilityHandlers
    , fired :: IORef [Txt]
    , ticking :: IORef Bool
    , ref :: IORef (Maybe JSV)
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
    { direction :: Direction
    , height :: Double
    , width :: Double
    , top :: Double
    , bottom :: Double
    , percentagePassed :: Double
    , pixelsPassed :: Double
    , bottomPassed :: Bool
    , bottomVisible :: Bool
    , fits :: Bool
    , passing :: Bool
    , offScreen :: Bool
    , onScreen :: Bool
    , topPassed :: Bool
    , topVisible :: Bool
    } deriving (Generic,Default)

instance Typeable ms => Pure Visibility ms where
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

                    (bottom,height,top,width) <- boundingRect (Element r)

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
                        ( ClassList classes 
                        : HostRef handleRef
                        : attributes
                        )
                        children

                }

instance HasAsProp (Visibility ms) where
    type AsProp (Visibility ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a v = v { as = a }

instance HasAttributesProp (Visibility ms) where
    type Attribute (Visibility ms) = Feature ms
    getAttributes = attributes
    setAttributes as v = v { attributes = as }

instance HasChildrenProp (Visibility ms) where
    type Child (Visibility ms) = View ms
    getChildren = children
    setChildren cs v = v { children = cs }

instance HasClassesProp (Visibility ms) where
    getClasses = classes
    setClasses cs v = v { classes = cs }

instance HasContextProp (Visibility ms) where
    type ContextProp (Visibility ms) = Maybe JSV
    getContext = context
    setContext c v = v { context = c }

instance HasContinuousProp (Visibility ms) where
    getContinuous = continuous
    setContinuous c v = v { continuous = c }

instance HasFireOnMountProp (Visibility ms) where
    getFireOnMount = fireOnMount
    setFireOnMount fom v = v { fireOnMount = fom }

instance HasOnBottomPassedProp (Visibility ms) where
    type OnBottomPassedProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnBottomPassed = onBottomPassed
    setOnBottomPassed obp v = v { onBottomPassed = obp }

instance HasOnBottomPassedReverseProp (Visibility ms) where
    type OnBottomPassedReverseProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnBottomPassedReverse = onBottomPassedReverse
    setOnBottomPassedReverse obpr v = v { onBottomPassedReverse = obpr }

instance HasOnBottomVisibleProp (Visibility ms) where
    type OnBottomVisibleProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnBottomVisible = onBottomVisible
    setOnBottomVisible obv v = v { onBottomVisible = obv }

instance HasOnBottomVisibleReverseProp (Visibility ms) where
    type OnBottomVisibleReverseProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnBottomVisibleReverse = onBottomVisibleReverse
    setOnBottomVisibleReverse obvr v = v { onBottomVisibleReverse = obvr }

instance HasOffsetProp (Visibility ms) where
    type OffsetProp (Visibility ms) = (Double,Double)
    getOffset = offset
    setOffset o v = v { offset = o }

instance HasOnceProp (Visibility ms) where
    getOnce = once
    setOnce o v = v { once = o }

instance HasOnPassedProp (Visibility ms) where
    type OnPassedProp (Visibility ms) = [(Calculations -> Ef ms IO (),Passed)]
    getOnPassed = onPassed
    setOnPassed op v = v { onPassed = op }

instance HasOnPassingProp (Visibility ms) where
    type OnPassingProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnPassing = onPassing
    setOnPassing op v = v { onPassing = op }

instance HasOnPassingReverseProp (Visibility ms) where
    type OnPassingReverseProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnPassingReverse = onPassingReverse
    setOnPassingReverse opr v = v { onPassingReverse = opr }

instance HasOnOffScreenProp (Visibility ms) where
    type OnOffScreenProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnOffScreen = onOffScreen
    setOnOffScreen oos v = v { onOffScreen = oos }

instance HasOnOnScreenProp (Visibility ms) where
    type OnOnScreenProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnOnScreen = onOnScreen
    setOnOnScreen oos v = v { onOnScreen = oos }

instance HasOnTopPassedProp (Visibility ms) where
    type OnTopPassedProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnTopPassed = onTopPassed
    setOnTopPassed otp v = v { onTopPassed = otp }

instance HasOnTopPassedReverseProp (Visibility ms) where
    type OnTopPassedReverseProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnTopPassedReverse = onTopPassedReverse
    setOnTopPassedReverse otpr v = v { onTopPassedReverse = otpr }

instance HasOnTopVisibleProp (Visibility ms) where
    type OnTopVisibleProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnTopVisible = onTopVisible
    setOnTopVisible otv v = v { onTopVisible = otv }

instance HasOnTopVisibleReverseProp (Visibility ms) where
    type OnTopVisibleReverseProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnTopVisibleReverse = onTopVisibleReverse 
    setOnTopVisibleReverse otvr v = v { onTopVisibleReverse = otvr }

instance HasOnUpdateProp (Visibility ms) where
    type OnUpdateProp (Visibility ms) = Maybe (Calculations -> Ef ms IO ())
    getOnUpdate = onUpdate
    setOnUpdate ou v = v { onUpdate = ou }