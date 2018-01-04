{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Transition.TransitionGroup where

-- Dependency hierarchy is reversed here. TransitionGroup depends upon Transition for 
-- component inspection so Transition cannot export TransitionGroup. The solution
-- is to either import Transition and Transition.TransitionGroup or to import
-- Semantic or more specifically Semantic.Modules to get both of them.

import Data.Maybe (isJust,fromJust,fromMaybe)
import GHC.Generics as G
import Pure.View hiding (animation,lookup,visible)

import Semantic.Utils

import Semantic.Modules.Transition

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Animation
import Semantic.Properties.AnimationDuration

data TransitionGroup ms = TransitionGroup_
    { as :: [Feature ms] -> [(Int,View ms)] -> View ms
    , attributes :: [Feature ms]
    , children :: [(Int,View ms)]
    , classes :: [Txt]
    , animation :: Txt
    , duration :: AnimationDuration
    } deriving (Generic)

instance Default (TransitionGroup ms) where
    def = (G.to gdef :: TransitionGroup ms) 
        { as = list Div
        , animation = "fade"
        , duration = Uniform 500 
        }

pattern TransitionGroup :: VC ms => TransitionGroup ms -> View ms
pattern TransitionGroup tg = View tg

data TransitionGroupState ms = TGS
    { buffer :: [(Int,View ms)]
    }

instance VC ms => Pure TransitionGroup ms where
    render tg =
        Component "Semantic.Modules.Transition.TransitionGroup" tg $ \self ->
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
                    tg@TransitionGroup_ {..} <- getProps self
                    return TGS 
                        { buffer = map (wrapChild animation duration True False) children
                        }

                , receiveProps = \TransitionGroup_ { animation = anim, duration = dur, children = cs } TGS {..} -> return TGS
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
                    
                , renderer = \TransitionGroup_ {..} TGS {..} -> as attributes buffer
                }

instance HasAsProp (TransitionGroup ms) where
    type AsProp (TransitionGroup ms) = [Feature ms] -> [(Int,View ms)] -> View ms
    getAs = as
    setAs a tg = tg { as = a }

instance HasAttributesProp (TransitionGroup ms) where
    type Attribute (TransitionGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as tg = tg { attributes = as }

instance HasChildrenProp (TransitionGroup ms) where
    type Child (TransitionGroup ms) = (Int,View ms)
    getChildren = children
    setChildren cs tg = tg { children = cs }

instance HasClassesProp (TransitionGroup ms) where
    getClasses = classes
    setClasses cs tg = tg { classes = cs }

instance HasAnimationProp (TransitionGroup ms) where
    getAnimation = animation
    setAnimation a tg = tg { animation = a }

instance HasAnimationDurationProp (TransitionGroup ms) where
    getAnimationDuration = duration
    setAnimationDuration d tg = tg { duration = d }