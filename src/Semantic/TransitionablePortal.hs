{-# LANGUAGE UndecidableInstances #-}
module Semantic.TransitionablePortal
  ( module Properties
  , module Tools
  , module Portal
  , module Transition
  , TransitionablePortal(..), pattern TransitionablePortal
  ) where

import Pure hiding (Animation,Transition,OnClose,Open,animation,duration,not,(#))

import Control.Monad (void)
import Data.Maybe (isNothing)
import GHC.Generics as G

import Semantic.Utils

import Semantic.Portal as Portal
import Semantic.Transition as Transition

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern OnClose, OnClose(..)
  , pattern OnHide, OnHide(..)
  , pattern OnOpen, OnOpen(..)
  , pattern OnStart, OnStart(..)
  , pattern Open, Open(..)
  , pattern Animation, Animation(..)
  , pattern AnimationDuration, AnimationDuration(..)
  , pattern WithPortal, WithPortal(..)
  , pattern WithTransition, WithTransition(..)
  , pattern TransitionOnMount, TransitionOnMount(..)
  , pattern Visible, Visible(..)
  , AnimationDuration(..)
  )

import Data.Function as Tools ((&))

data TransitionablePortal = TransitionablePortal_
    { children       :: [View]
    , onClose        :: IO ()
    , onHide         :: TransitionStatus -> IO ()
    , onOpen         :: IO ()
    , onStart        :: TransitionStatus -> IO ()
    , open           :: Maybe Bool
    , animation      :: Txt
    , duration       :: AnimationDuration
    , withPortal     :: Portal -> Portal
    , withTransition :: Transition -> Transition
    } deriving (Generic)

instance Default TransitionablePortal where
    def = (G.to gdef)
        { animation      = "scale"
        , duration       = Uniform 400
        , withPortal     = id
        , withTransition = id
        }

pattern TransitionablePortal :: TransitionablePortal -> TransitionablePortal
pattern TransitionablePortal tp = tp

data TransitionablePortalState = TPS
    { portalOpen        :: Bool
    , transitionVisible :: Bool
    }

instance Pure TransitionablePortal where
    view =
        Component $ \self ->
            let
                handlePortalClose = do
                    TransitionablePortal_ {..} <- ask self
                    TPS {..} <- get self
                    (isNothing open) #
                        modify_ self (\_ TPS {..} -> TPS { portalOpen = not portalOpen, .. })

                handlePortalOpen _ = modify_ self $ \_ TPS {..} -> TPS { portalOpen = True, .. }

                handleTransitionHide status = do
                    TransitionablePortal_ {..} <- ask self
                    TPS {..} <- get self
                    modify self $ \_ TPS {..} -> TPS { transitionVisible = False, .. }
                    onClose
                    onHide status

                handleTransitionStart status = do
                    TransitionablePortal_ {..} <- ask self
                    TPS {..} <- get self
                    onStart status
                    (status == Entering) # do
                        modify_ self (\_ TPS {..} -> TPS { transitionVisible = True, .. })
                        onOpen

            in def
                { construct = return (TPS def def)

                , receive = \newprops oldstate -> return $
                    case open (newprops :: TransitionablePortal) of
                        Just o -> oldstate { portalOpen = o }
                        _      -> oldstate

                , render = \TransitionablePortal_ {..} TPS {..} ->
                    View $ Portal.Portal $ withPortal $ def
                        & Open (portalOpen || transitionVisible)
                        & OnOpen handlePortalOpen
                        & OnClose handlePortalClose
                        & Children
                            [ View $ Transition $ withTransition $ def
                                & TransitionOnMount True
                                & OnStart handleTransitionStart
                                & OnHide handleTransitionHide
                                & Visible portalOpen
                                & Animation animation
                                & AnimationDuration duration
                                & Children children
                            ]
                }

instance HasChildren TransitionablePortal where
    getChildren = children
    setChildren cs tp = tp { children = cs }

instance HasProp OnClose TransitionablePortal where
    type Prop OnClose TransitionablePortal = IO ()
    getProp _ = onClose
    setProp _ oc tp = tp { onClose = oc }

instance HasProp OnHide TransitionablePortal where
    type Prop OnHide TransitionablePortal = TransitionStatus -> IO ()
    getProp _ = onHide
    setProp _ oh tp = tp { onHide = oh }

instance HasProp OnOpen TransitionablePortal where
    type Prop OnOpen TransitionablePortal = IO ()
    getProp _ = onOpen
    setProp _ oo tp = tp { onOpen = oo }

instance HasProp OnStart TransitionablePortal where
    type Prop OnStart TransitionablePortal = TransitionStatus -> IO ()
    getProp _ = onStart
    setProp _ os tp = tp { onStart = os }

instance HasProp Open TransitionablePortal where
    type Prop Open TransitionablePortal = Maybe Bool
    getProp _ = open
    setProp _ o tp = tp { open = o }

instance HasProp Animation TransitionablePortal where
    type Prop Animation TransitionablePortal = Txt
    getProp _ = animation
    setProp _ a tp = tp { animation = a }

instance HasProp AnimationDuration TransitionablePortal where
    type Prop AnimationDuration TransitionablePortal = AnimationDuration
    getProp _ = duration
    setProp _ ad tp = tp { duration = ad }

instance HasProp WithPortal TransitionablePortal where
    type Prop WithPortal TransitionablePortal = Portal -> Portal
    getProp _ = withPortal
    setProp _ wp tp = tp { withPortal = wp }

instance HasProp WithTransition TransitionablePortal where
    type Prop WithTransition TransitionablePortal = Transition -> Transition
    getProp _ = withTransition
    setProp _ wt tp = tp { withTransition = wt }
