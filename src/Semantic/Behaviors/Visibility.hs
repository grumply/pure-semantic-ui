module Semantic.Behaviors.Visibility where

import Data.Coerce
import Data.IORef
import GHC.Generics as G
import Pure.View
import Pure.Lifted
import Pure.DOM (addAnimation)

import Semantic.Utils

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
    , onBottomPassed :: Maybe (Calculations -> Ef ms IO ())
    , onBottomPassedReverse :: Maybe (Calculations -> Ef ms IO ())
    , onBottomVisible :: Maybe (Calculations -> Ef ms IO ())
    , onBottomVisibleReverse :: Maybe (Calculations -> Ef ms IO ())
    , offset :: (Double,Double)
    , once :: Bool
    , onPassed :: [(Calculations -> Ef ms IO (),Passed)]
    , onPassing :: Maybe (Calculations -> Ef ms IO ())
    , onPassingReverse :: Maybe (Calculations -> Ef ms IO ())
    , onOffScreen :: Maybe (Calculations -> Ef ms IO ())
    , onOnScreen :: Maybe (Calculations -> Ef ms IO ())
    , onTopPassed :: Maybe (Calculations -> Ef ms IO ())
    , onTopPassedReverse :: Maybe (Calculations -> Ef ms IO ())
    , onTopVisible :: Maybe (Calculations -> Ef ms IO ())
    , onTopVisibleReverse :: Maybe (Calculations -> Ef ms IO ())
    , onUpdate :: Maybe (Calculations -> Ef ms IO ())
    } deriving (Generic)

instance Default (Visibility ms) where
    def = (G.to gdef) { context = Just (coerce window), once = True }

pattern Visibility :: Typeable ms => Visibility ms -> View ms
pattern Visibility v = View v

data VisibilityState = VS
    { oldCalculations :: IORef Calculations
    , calculations :: IORef Calculations
    , verticalOffset :: IORef Int
    , handlers :: IORef VisibilityHandlers
    , fired :: IORef [Txt]
    , ticking :: IORef Bool
    , ref :: IORef JSV
    }

data VisibilityHandlers = VH
    { resizeHandler :: IO ()
    , scrollHandler :: IO ()
    } deriving (Generic,Default)

data Calculations = Calculations
    { percentagePassed :: Double
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
                    writeIORef ref n
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

                    r <- readIORef ref

                    (bottom,height,top,width) <- boundingRect (Element r)

                    oldPYO <- readIORef verticalOffset
                    newPYO <- pageYOffset
                    ih     <- fromIntegral <$> innerHeight

                    let (topOffset,bottomOffset) = offset

                        direction    = (newPYO > oldPYO) ? "down" $ "up"
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
                                 <*> newIORef undefined

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
