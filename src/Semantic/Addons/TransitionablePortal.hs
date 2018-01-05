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

                handlePortalOpen = void $ setState self $ \_ TPS {..} -> TPS { portalOpen = True, .. }

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