{-# LANGUAGE UndecidableInstances #-}
module Semantic.Transition
  ( module Properties
  , module Tools
  , Transition(..), pattern Transition
  , Group(..), pattern Group
  , TransitionStatus(..)
  ) where

import Pure hiding (Transition,animation,visible,duration,not,(#))

import Control.Concurrent
import Control.Monad (void)
import Data.Foldable (for_)
import Data.IORef
import Data.Maybe
import Data.Monoid
import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
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

data TransitionStatus = Unmounted | Entered | Entering | Exited | Exiting
    deriving (Generic,Default,Ord,Eq)

calculateTransitionDuration :: TransitionStatus -> AnimationDuration -> Int
calculateTransitionDuration _        (Uniform d) = d
calculateTransitionDuration Exiting  Skewed {..} = hide
calculateTransitionDuration _        Skewed {..} = show

data Transition = Transition_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , animation :: Txt
    , duration :: AnimationDuration
    , visible :: Bool
    , mountOnShow :: Bool
    , onComplete :: TransitionStatus -> IO ()
    , onHide :: TransitionStatus -> IO ()
    , onShow :: TransitionStatus -> IO ()
    , onStart :: TransitionStatus -> IO ()
    , transitionOnMount :: Bool
    , unmountOnHide :: Bool
    } deriving (Generic)

instance Default Transition where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
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

instance Pure Transition where
    view =
        Component $ \self ->
            let

                setSafeState f = do
                    TS {..} <- get self
                    mtd <- readIORef mounted
                    mtd # modifyM_ self (const f)

                handleStart = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self

                    upcoming <- readIORef next
                    writeIORef next def

                    setSafeState $ \TS {..} -> return
                        ( TS { status = fromMaybe status upcoming
                             , animating = True
                             , ..
                             }
                        , for_ upcoming $ \s -> do
                            onStart s
                            tid <- forkIO $ do
                                threadDelay (calculateTransitionDuration s duration * 1000)
                                handleComplete
                            writeIORef transitionTimeout (Just tid)
                        )

                handleComplete = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self
                    onComplete status
                    maybe (return ()) (const handleStart) =<< readIORef next
                    s <- computeCompletedStatus
                    let callback = (status == Entering) ? onShow $ onHide
                    setSafeState $ \TS {..} -> return
                        ( TS { status = s, animating = False, .. }
                        , callback s
                        )

                updateStatus = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self
                    upcoming <- readIORef next
                    (isJust upcoming) # do
                        writeIORef next . Just =<< computeNextStatus
                        (not animating) # handleStart

                computeCompletedStatus = do
                    Transition_ {..} <- ask self
                    TS          {..} <- get self

                    return $
                      (status == Entering)
                        ? Entered
                        $ unmountOnHide
                            ? Unmounted
                            $ Exited

                computeInitialStatuses = do
                    Transition_ {..} <- ask self
                    return $
                        if | visible && transitionOnMount -> (Exited   ,Just Entering)
                           | visible                      -> (Entered  ,Nothing)
                           | mountOnShow || unmountOnHide -> (Unmounted,Nothing)
                           | otherwise                    -> (Exited   ,Nothing)

                computeNextStatus = do
                    TS {..} <- get self
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
                        TS {..} <- get self
                        writeIORef mounted True
                        updateStatus

                    , receive = \newprops oldstate -> do
                        oldprops <- ask self
                        TS {..}  <- get self
                        let (current,upcoming) = computeStatuses (visible newprops) status
                        writeIORef next upcoming
                        let newStatus = fromMaybe status current
                        return TS
                            { status = newStatus
                            , ..
                            }

                    , updated = \_ _ ->
                        updateStatus

                    , unmounted = do
                        TS {..} <- get self
                        writeIORef mounted False

                    , render = \Transition_ {..} TS {..} ->
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
                                              Entering -> (animation-"duration",fromIntegral (calculateTransitionDuration status duration) ms)
                                              Exiting  -> (animation-"duration",fromIntegral (calculateTransitionDuration status duration) ms)
                                              _        -> def
                                  in styles <> [ ad ]

                          in
                              (status /= Unmounted) #
                                    as (features & Classes (animationClasses []) & Styles (animationStyles [])) children

                    }

instance HasProp As Transition where
    type Prop As Transition = Features -> [View] -> View
    getProp _ = as
    setProp _ a t = t { as = a }

instance HasChildren Transition where
    getChildren = children
    setChildren cs t = t { children = cs }

instance HasFeatures Transition where
    getFeatures = features
    setFeatures fs t = t { features = fs }

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
    type Prop OnComplete Transition = TransitionStatus -> IO ()
    getProp _ = onComplete
    setProp _ oc t = t { onComplete = oc }

instance HasProp OnHide Transition where
    type Prop OnHide Transition = TransitionStatus -> IO ()
    getProp _ = onHide
    setProp _ oh t = t { onHide = oh }

instance HasProp OnShow Transition where
    type Prop OnShow Transition = TransitionStatus -> IO ()
    getProp _ = onShow
    setProp _ os t = t { onShow = os }

instance HasProp OnStart Transition where
    type Prop OnStart Transition = TransitionStatus -> IO ()
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
    def = (G.to gdef :: Group)
        { as = \fs cs -> (Keyed Div) & Features fs & KeyedChildren cs
        , animation = "fade"
        , duration = Uniform 500
        }

pattern Group :: Group -> Group
pattern Group tg = tg

data GroupState = TGS
    { buffer :: [(Int,View)]
    }

instance Pure Group where
    view =
        Component $ \self ->
            let
                handleOnHide key _ =
                    modify_ self $ \_ TGS {..} -> TGS { buffer = Prelude.filter ((/= key) . fst) buffer, .. }

                wrapChild anim dur vis tom (key,child) =
                    (key,View $ Transition def
                        { animation = anim
                        , duration = dur
                        , transitionOnMount = tom
                        , visible = vis
                        , onHide = handleOnHide key
                        , children = [ child ]
                        }
                    )

                hide :: View -> View
                hide (View Transition_ {..}) = View Transition_ { visible = False, .. }

                fromTransition (Just (View t@Transition_ {})) f = Just (f t)
                fromTransition _ _ = Nothing

            in def
                { construct = do
                    tg@Group_ {..} <- ask self
                    return TGS
                        { buffer = fmap (wrapChild animation duration True False) children
                        }

                , receive = \Group_ { animation = anim, duration = dur, children = cs } TGS {..} -> return TGS
                    { buffer = flip fmap (mergeMappings buffer cs) $ \(k,c) ->
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

                , render = \Group_ {..} TGS {..} -> as features buffer
                }

instance HasProp As Group where
    type Prop As Group = Features -> [(Int,View)] -> View
    getProp _ = as
    setProp _ a tg = tg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as tg = tg { features = as }

instance HasKeyedChildren Group where
    getKeyedChildren = children
    setKeyedChildren cs tg = tg { children = cs }

instance HasProp Animation Group where
    type Prop Animation Group = Txt
    getProp _ = animation
    setProp _ a tg = tg { animation = a }

instance HasProp AnimationDuration Group where
    type Prop AnimationDuration Group = AnimationDuration
    getProp _ = duration
    setProp _ d tg = tg { duration = d }
