module Semantic.Modules.Transition where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (animation,onComplete,visible)

import Semantic.Utils

import Semantic.Properties.Children
import Semantic.Properties.Animation
import Semantic.Properties.AnimationDuration
import Semantic.Properties.Visible
import Semantic.Properties.MountOnShow
import Semantic.Properties.OnComplete
import Semantic.Properties.OnHide
import Semantic.Properties.OnShow
import Semantic.Properties.OnStart
import Semantic.Properties.TransitionOnMount
import Semantic.Properties.UnmountOnHide

data TransitionStatus = Unmounted | Entered | Entering | Exited | Exiting
    deriving (Generic,Default,Ord,Eq)

calculateTransitionDuration :: TransitionStatus -> AnimationDuration -> Int
calculateTransitionDuration _        (Uniform d) = d
calculateTransitionDuration Exiting  Skewed {..} = hide
calculateTransitionDuration _        Skewed {..} = show

data Transition ms = Transition_
    { children :: [View ms]
    , animation :: Txt
    , duration :: AnimationDuration
    , visible :: Bool
    , mountOnShow :: Bool
    , onComplete :: TransitionStatus -> Ef ms IO ()
    , onHide :: TransitionStatus -> Ef ms IO ()
    , onShow :: TransitionStatus -> Ef ms IO ()
    , onStart :: TransitionStatus -> Ef ms IO ()
    , transitionOnMount :: Bool
    , unmountOnHide :: Bool
    } deriving (Generic)

instance Default (Transition ms) where
    def = (G.to gdef)
        { animation = "fade"
        , duration = Uniform 500
        , visible = True
        , mountOnShow = True
        }

pattern Transition :: Transition ms -> View ms
pattern Transition t = View t

data TransitionState = TS
    { status :: TransitionStatus
    , animating :: Bool
    , mounted :: IORef Bool
    , next :: IORef (Maybe TransitionStatus)
    , transitionTimeout :: IORef (Maybe ThreadId)
    }

instance Pure Transition ms where
    render t =
        Component "Semantic.Modules.Transition" t $ \self ->
            let

                setSafeState f = do
                    TS {..} <- getState self
                    mtd <- readIORef mounted
                    mtd # void (setStateIO self (\_ -> return . f))

                handleStart = do
                    Transition_ {..} <- getProps self
                    TS          {..} <- getState self

                    upcoming <- readIORef next
                    writeIORef next def

                    setSafeState $ \TS {..} ->
                        ( TS { status = fromMaybe status upcoming
                             , animating = True
                             , ..
                             }
                        , for_ upcoming $ \s -> do
                            parent self (onStart s)
                            tid <- forkIO $ do
                                threadDelay (calculateTransitionDuration s duration * 1000)
                                handleComplete
                            writeIORef transitionTimeout (Just tid)
                        )

                handleComplete = do
                    Transition_ {..} <- getProps self
                    TS          {..} <- getState self
                    parent self (onComplete status)
                    maybe (return ()) (const handleStart) =<< readIORef next
                    s <- computeCompletedStatus
                    let callback = (status == Entering) ? onShow $ onHide
                    setSafeState $ \TS {..} ->
                        ( TS { status = s, animating = False, .. }
                        , void (parent self (callback s))
                        )

                updateStatus = do
                    Transition_ {..} <- getProps self
                    TS          {..} <- getState self
                    upcoming <- readIORef next
                    upcoming # do
                        writeIORef next . Just =<< computeNextStatus
                        (not animating) # handleStart

                computeCompletedStatus = do
                    Transition_ {..} <- getProps self
                    TS          {..} <- getState self

                    return $
                      (status == Entering)
                        ? Entered
                        $ unmountOnHide
                            ? Unmounted
                            $ Exited

                computeInitialStatuses = do
                    Transition_ {..} <- getProps self
                    return $
                        if | visible && transitionOnMount -> (Exited   ,Just Entering)
                           | visible                      -> (Entered  ,Nothing)
                           | mountOnShow || unmountOnHide -> (Unmounted,Nothing)
                           | otherwise                    -> (Exited   ,Nothing)

                computeNextStatus = do
                    TS {..} <- getState self
                    return $
                        if animating
                            then (status == Entering) ? Exiting $ Entering
                            else (status == Entered)  ? Exiting $ Entering

                computeStatuses True status =
                    ( (status == Unmounted) ? Just Exited $ Nothing
                    , (status /= Entering && status /= Entered) ? Just Entering $ Nothing
                    )
                computeStatuses _ status =
                    ( Nothing, (status == Entering || status == Entered) ? Just Exiting $ Nothing)

            in
                def
                    { construct = do
                        (status,next) <- computeInitialStatuses
                        TS status def <$> newIORef def <*> newIORef next <*> newIORef def

                    , mounted = do
                        TS {..} <- getState self
                        writeIORef mounted True
                        updateStatus

                    , receiveProps = \newprops oldstate -> do
                        oldprops <- getProps self
                        TS {..}  <- getState self
                        let (current,upcoming) = computeStatuses (visible newprops) status
                        writeIORef next upcoming
                        let newStatus = fromMaybe status current
                        return TS
                            { status = newStatus
                            , ..
                            }

                    , updated = \_ _ _ ->
                        updateStatus

                    , unmount = do
                        TS {..} <- getState self
                        writeIORef mounted False

                    , renderer = \Transition_ {..} TS {..} ->
                          let
                              animationClasses =
                                  ( animation ) :
                                      if animation `elem` directionalTransitions
                                          then
                                              [ animating # "animating"
                                              , case status of
                                                  Entering -> "in"
                                                  Exiting  -> "out"
                                                  Exited   -> "hidden"
                                                  _        -> def
                                              , (status /= Exited) # "visible"
                                              , "transition"
                                              ]
                                          else
                                              [ animating # "animating transition" ]

                              animationStyles =
                                  let ad =
                                          case status of
                                              Entering -> ("animation-duration",ms(calculateTransitionDuration status duration))
                                              Exiting  -> ("animation-duration",ms(calculateTransitionDuration status duration))
                                              _        -> def
                                  in [ ad ]

                          in
                              (status /= Unmounted) #
                                  Div [ ClassList animationClasses, StyleList animationStyles ]
                                      children

                    }

instance HasChildrenProp (Transition ms) where
    type Child (Transition ms) = View ms
    getChildren = children
    setChildren cs t = t { children = cs }

instance HasAnimationProp (Transition ms) where
    getAnimation = animation
    setAnimation a t = t { animation = a }

instance HasAnimationDurationProp (Transition ms) where
    getAnimationDuration = duration
    setAnimationDuration d t = t { duration = d }

instance HasVisibleProp (Transition ms) where
    getVisible = visible
    setVisible v t = t { visible = v }

instance HasMountOnShowProp (Transition ms) where
    getMountOnShow = mountOnShow
    setMountOnShow mos t = t { mountOnShow = mos }

instance HasOnCompleteProp (Transition ms) where
    type OnCompleteProp (Transition ms) = TransitionStatus -> Ef ms IO ()
    getOnComplete = onComplete
    setOnComplete oc t = t { onComplete = oc }

instance HasOnHideProp (Transition ms) where
    type OnHideProp (Transition ms) = TransitionStatus -> Ef ms IO ()
    getOnHide = onHide
    setOnHide oh t = t { onHide = oh }

instance HasOnShowProp (Transition ms) where
    type OnShowProp (Transition ms) = TransitionStatus -> Ef ms IO ()
    getOnShow = onShow
    setOnShow os t = t { onShow = os }

instance HasOnStartProp (Transition ms) where
    type OnStartProp (Transition ms) = TransitionStatus -> Ef ms IO ()
    getOnStart = onStart
    setOnStart os t = t { onStart = os }

instance HasTransitionOnMountProp (Transition ms) where
    getTransitionOnMount = transitionOnMount
    setTransitionOnMount tom t = t { transitionOnMount = tom }

instance HasUnmountOnHideProp (Transition ms) where
    getUnmountOnHide = unmountOnHide
    setUnmountOnHide uoh t = t { unmountOnHide = uoh }
