{-# LANGUAGE UndecidableInstances #-}
module Semantic.Transition
  ( module Properties
  , module Tools
  , Transition(..), pattern Transition
  , Group(..), pattern Group
  , TransitionStatus(..)
  ) where

import Control.Concurrent
import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (animation,onComplete,visible,lookup)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Animation, Animation(..)
  , pattern AnimationDuration, AnimationDuration(..)
  , pattern Visible, Visible(..)
  , pattern MountOnShow, MountOnShow(..)
  , pattern OnComplete, OnComplete(..)
  , pattern OnHide, OnHide(..)
  , pattern OnShow, OnShow(..)
  , pattern OnStart, OnStart(..)
  , pattern TransitionOnMount, TransitionOnMount(..)
  , pattern UnmountOnHide, UnmountOnHide(..)
  , AnimationDuration(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data TransitionStatus = Unmounted | Entered | Entering | Exited | Exiting
    deriving (Generic,Default,Ord,Eq)

calculateTransitionDuration :: TransitionStatus -> AnimationDuration -> Int
calculateTransitionDuration _        (Uniform d) = d
calculateTransitionDuration Exiting  Skewed {..} = hide
calculateTransitionDuration _        Skewed {..} = show

data Transition = Transition_
    { as :: Maybe (Features -> [View] -> View)
    , children :: [View]
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

instance Default Transition where
    def = (G.to gdef)
        { as = Just Div
        , animation = "fade"
        , duration = Uniform 500
        , visible = True
        , mountOnShow = True
        }

pattern Transition :: Transition -> Transition
pattern Transition t = t

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
                              animationClasses cs =
                                  ( animation : cs ) ++
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

                              animationStyles styles =
                                  let ad =
                                          case status of
                                              Entering -> ("animation-duration",ms(calculateTransitionDuration status duration))
                                              Exiting  -> ("animation-duration",ms(calculateTransitionDuration status duration))
                                              _        -> def
                                  in styles <> [ ad ]

                              headMay [] = nil
                              headMay (x : _) = x

                          in
                              (status /= Unmounted) #
                                  case as of
                                    Nothing -> updateStylesAndClasses animationStyles animationClasses $ headMay children
                                    Just w  -> w [ ClassList (animationClasses []), StyleList (animationStyles [])] children

                    }

instance HasChildren Transition where
    getChildren = children
    setChildren cs t = t { children = cs }

instance HasProp Animation Transition where
    type Prop Animation Transition = Txt
    getProp _ = animation
    setProp _ a t = t { animation = a }

instance HasProp AnimationDuration Transition where
    type Prop AnimationDuration Transition = AnimationDuration
    getProp _ = duration
    setProp _ d t = t { duration = d }

instance HasProp Visible Transition where
    type Prop Visible Transition = Bool
    getProp _ = visible
    setProp _ v t = t { visible = v }

instance HasProp MountOnShow Transition where
    type Prop MountOnShow Transition = Bool
    getProp _ = mountOnShow
    setProp _ mos t = t { mountOnShow = mos }

instance HasProp OnComplete Transition where
    type Prop OnComplete Transition = TransitionStatus -> Ef ms IO ()
    getProp _ = onComplete
    setProp _ oc t = t { onComplete = oc }

instance HasProp OnHide Transition where
    type Prop OnHide Transition = TransitionStatus -> Ef ms IO ()
    getProp _ = onHide
    setProp _ oh t = t { onHide = oh }

instance HasProp OnShow Transition where
    type Prop OnShow Transition = TransitionStatus -> Ef ms IO ()
    getProp _ = onShow
    setProp _ os t = t { onShow = os }

instance HasProp OnStart Transition where
    type Prop OnStart Transition = TransitionStatus -> Ef ms IO ()
    getProp _ = onStart
    setProp _ os t = t { onStart = os }

instance HasProp TransitionOnMount Transition where
    type Prop TransitionOnMount Transition = Bool
    getProp _ = transitionOnMount
    setProp _ tom t = t { transitionOnMount = tom }

instance HasProp UnmountOnHide Transition where
    type Prop UnmountOnHide Transition = Bool
    getProp _ = unmountOnHide
    setProp _ uoh t = t { unmountOnHide = uoh }

data Group = Group_
    { as :: Features -> [(Int,View)] -> View
    , features :: Features
    , children :: [(Int,View)]
    , classes :: [Txt]
    , animation :: Txt
    , duration :: AnimationDuration
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef :: Group ms)
        { as = list Div
        , animation = "fade"
        , duration = Uniform 500
        }

pattern Group :: Group -> Group
pattern Group tg = tg

data GroupState = TGS
    { buffer :: [(Int,View)]
    }

instance VC => Pure Group ms where
    render tg =
        Component "Semantic.Modules.Transition.Group" tg $ \self ->
            let
                handleOnHide key _ =
                    void $ setState self $ \_ TGS {..} ->
                        TGS { buffer = filter ((/= key) . fst) buffer, .. }

                wrapChild anim dur vis tom (key,child) =
                    (key,Transition def
                        { animation = anim
                        , duration = dur
                        , transitionOnMount = tom
                        , visible = vis
                        , onHide = handleOnHide key
                        , children = [ child ]
                        }
                    )

                hide (View Transition_ {..}) = View Transition_ { visible = False, .. }

                fromTransition (Just (View t@Transition_ {})) f = Just (f t)
                fromTransition _ _ = Nothing

            in def
                { construct = do
                    tg@Group_ {..} <- getProps self
                    return TGS
                        { buffer = map (wrapChild animation duration True False) children
                        }

                , receiveProps = \Group_ { animation = anim, duration = dur, children = cs } TGS {..} -> return TGS
                    { buffer = flip map (mergeMappings buffer cs) $ \(k,c) ->
                        let prevChild = lookup k buffer
                            hasPrev   = isJust prevChild
                            hasNext   = isJust (lookup k cs)
                            leaving   = fromMaybe False (fromTransition prevChild (not . visible))
                            entering  = hasNext && (not hasPrev || leaving)
                            exiting   = not hasNext && hasPrev && not leaving

                        in if | entering  -> wrapChild anim dur True True (k,c)
                              | exiting   -> (k,hide (fromJust prevChild))
                              | otherwise -> fromJust $ fromTransition prevChild $ \Transition_ {..} ->
                                                 wrapChild anim dur visible transitionOnMount (k,c)

                    , ..
                    }

                , renderer = \Group_ {..} TGS {..} -> as attributes buffer
                }

instance HasProp As Group where
    type Prop As Group = Features -> [(Int,View)] -> View
    getProp _ = as
    setProp _ a tg = tg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as tg = tg { features = as }

instance HasChildren Group where
    getChildren = children
    setChildren cs tg = tg { children = cs }

instance HasProp Classes Group where
    type Prop Classes Group = [Txt]
    getProp _ = classes
    setProp _ cs tg = tg { classes = cs }

instance HasProp Animation Group where
    type Prop Animation Group = Txt
    getProp _ = animation
    setProp _ a tg = tg { animation = a }

instance HasProp AnimationDuration Group where
    type Prop AnimationDuration Group = AnimationDuration
    getProp _ = duration
    setProp _ d tg = tg { duration = d }
