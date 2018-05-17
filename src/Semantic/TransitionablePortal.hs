{-# LANGUAGE UndecidableInstances #-}
module Semantic.TransitionablePortal
  ( module Properties
  , module Tools
  , module Portal
  , module Transition
  , TransitionablePortal(..), pattern TransitionablePortal
  ) where

import GHC.Generics as G
import Pure.View hiding (animation,OnClose)

import Semantic.Utils

import Semantic.Portal as Portal
import Semantic.Transition as Transition

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Children, Children(..)
  , pattern OnClose, OnClose(..)
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
import Pure.Data.Default as Tools

data TransitionablePortal = TransitionablePortal_
    { children       :: [View]
    , onClose        :: Ef ms IO ()
    , onHide         :: TransitionStatus -> Ef ms IO ()
    , onOpen         :: Ef ms IO ()
    , onStart        :: TransitionStatus -> Ef ms IO ()
    , open           :: Maybe Bool
    , animation      :: Txt
    , duration       :: AnimationDuration
    , withPortal     :: Portal ms -> Portal ms
    , withTransition :: Transition ms -> Transition ms
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

instance VC => Pure TransitionablePortal ms where
    render tp =
        Component "Semantic.Addons.TransitionablePortal" tp $ \self ->
            let
                handlePortalClose = do
                    TransitionablePortal_ {..} <- getProps self
                    TPS {..} <- getState self
                    isNil open # void (setState self $ \_ TPS {..} -> TPS { portalOpen = not portalOpen, .. })

                handlePortalOpen _ = void $ setState self $ \_ TPS {..} -> TPS { portalOpen = True, .. }

                handleTransitionHide status = do
                    TransitionablePortal_ {..} <- getProps self
                    TPS {..} <- getState self
                    setState self $ \_ TPS {..} ->
                        TPS { transitionVisible = False, .. }
                    onClose
                    onHide status

                handleTransitionStart status = do
                    TransitionablePortal_ {..} <- getProps self
                    TPS {..} <- getState self
                    onStart status
                    (status == Entering) # do
                        setState self $ \_ TPS {..} ->
                            TPS { transitionVisible = True, .. }
                        onOpen

            in def
                { construct = return (TPS def def)

                , receiveProps = \newprops oldstate -> return $
                    case open (newprops :: TransitionablePortal ms) of
                        Just o -> oldstate { portalOpen = o }
                        _      -> oldstate

                , renderer = \TransitionablePortal_ {..} TPS {..} ->
                    Portal $ withPortal $ def
                        & Open (portalOpen || transitionVisible)
                        & OnOpen handlePortalOpen
                        & OnClose handlePortalClose
                        & Children
                            [ Transition $ withTransition $ def
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
    type Prop OnClose TransitionablePortal = Ef ms IO ()
    getProp _ = onClose
    setProp _ oc tp = tp { onClose = oc }

instance HasProp OnHide TransitionablePortal where
    type Prop OnHide TransitionablePortal = TransitionStatus -> Ef ms IO ()
    getProp _ = onHide
    setProp _ oh tp = tp { onHide = oh }

instance HasProp OnOpen TransitionablePortal where
    type Prop OnOpen TransitionablePortal = Ef ms IO ()
    getProp _ = onOpen
    setProp _ oo tp = tp { onOpen = oo }

instance HasProp OnStart TransitionablePortal where
    type Prop OnStart TransitionablePortal = TransitionStatus -> Ef ms IO ()
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
    type Prop WithPortal TransitionablePortal = Portal ms -> Portal ms
    getProp _ = withPortal
    setProp _ wp tp = tp { withPortal = wp }

instance HasProp WithTransition TransitionablePortal where
    type Prop WithTransition TransitionablePortal = Transition ms -> Transition ms
    getProp _ = withTransition
    setProp _ wt tp = tp { withTransition = wt }
