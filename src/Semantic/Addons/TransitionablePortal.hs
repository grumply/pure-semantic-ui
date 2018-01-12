{-# LANGUAGE UndecidableInstances #-}
module Semantic.Addons.TransitionablePortal where

import GHC.Generics as G
import Pure.View hiding (animation,OnClose)

import Semantic.Utils

import Semantic.Addons.Portal (Portal(),pattern Portal)
import Semantic.Modules.Transition (Transition(),pattern Transition,TransitionStatus(..))

import Semantic.Properties.Children
import Semantic.Properties.OnClose
import Semantic.Properties.OnHide
import Semantic.Properties.OnOpen
import Semantic.Properties.OnStart
import Semantic.Properties.Open
import Semantic.Properties.Animation
import Semantic.Properties.AnimationDuration
import Semantic.Properties.WithPortal
import Semantic.Properties.WithTransition

import Semantic.Properties.TransitionOnMount
import Semantic.Properties.Visible

data TransitionablePortal ms = TransitionablePortal_
    { children :: [View ms]
    , onClose :: Ef ms IO ()
    , onHide :: TransitionStatus -> Ef ms IO ()
    , onOpen :: Ef ms IO ()
    , onStart :: TransitionStatus -> Ef ms IO ()
    , open :: Maybe Bool
    , animation :: Txt
    , duration :: AnimationDuration
    , withPortal :: Portal ms -> Portal ms
    , withTransition :: Transition ms -> Transition ms
    } deriving (Generic)

instance Default (TransitionablePortal ms) where
    def = (G.to gdef) 
        { animation = "scale"
        , duration = Uniform 400 
        , withPortal = id
        , withTransition = id
        }

pattern TransitionablePortal :: VC ms => TransitionablePortal ms -> View ms
pattern TransitionablePortal tp = View tp

data TransitionablePortalState = TPS
    { portalOpen :: Bool
    , transitionVisible :: Bool
    }

instance VC ms => Pure TransitionablePortal ms where
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
                    case open newprops of
                        Just o -> oldstate { portalOpen = o }
                        _      -> oldstate

                , renderer = \TransitionablePortal_ {..} TPS {..} -> 
                    Portal $ withPortal $ def 
                        & Open (portalOpen || transitionVisible)
                        & OnOpen handlePortalOpen
                        & OnClose handlePortalClose
                        & Children 
                            [ Transition $ withTransition $ def
                                & TransitionOnMount
                                & OnStart handleTransitionStart
                                & OnHide handleTransitionHide
                                & Visible portalOpen
                                & Animation animation
                                & AnimationDuration duration
                                & Children children
                            ]
                }

instance HasChildrenProp (TransitionablePortal ms) where
    type Child (TransitionablePortal ms) = View ms
    getChildren = children
    setChildren cs tp = tp { children = cs }

instance HasOnCloseProp (TransitionablePortal ms) where
    type OnCloseProp (TransitionablePortal ms) = Ef ms IO ()
    getOnClose = onClose
    setOnClose oc tp = tp { onClose = oc }

instance HasOnHideProp (TransitionablePortal ms) where
    type OnHideProp (TransitionablePortal ms) = TransitionStatus -> Ef ms IO ()
    getOnHide = onHide
    setOnHide oh tp = tp { onHide = oh }

instance HasOnOpenProp (TransitionablePortal ms) where
    type OnOpenProp (TransitionablePortal ms) = Ef ms IO ()
    getOnOpen = onOpen
    setOnOpen oo tp = tp { onOpen = oo }

instance HasOnStartProp (TransitionablePortal ms) where
    type OnStartProp (TransitionablePortal ms) = TransitionStatus -> Ef ms IO ()
    getOnStart = onStart
    setOnStart os tp = tp { onStart = os }

instance HasOpenProp (TransitionablePortal ms) where
    type OpenProp (TransitionablePortal ms) = Maybe Bool
    getOpen = open
    setOpen o tp = tp { open = o }

instance HasAnimationProp (TransitionablePortal ms) where
    getAnimation = animation
    setAnimation a tp = tp { animation = a }

instance HasAnimationDurationProp (TransitionablePortal ms) where
    getAnimationDuration = duration
    setAnimationDuration ad tp = tp { duration = ad }

instance HasWithPortalProp (TransitionablePortal ms) where
    type WithPortalProp (TransitionablePortal ms) = Portal ms -> Portal ms
    getWithPortal = withPortal
    setWithPortal wp tp = tp { withPortal = wp }

instance HasWithTransitionProp (TransitionablePortal ms) where
    type WithTransitionProp (TransitionablePortal ms) = Transition ms -> Transition ms
    getWithTransition = withTransition
    setWithTransition wt tp = tp { withTransition = wt }