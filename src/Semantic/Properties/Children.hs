module Semantic.Properties.Children where

import Pure.View

import Semantic.Properties.Utils

class HasChildrenProp a where
    type Child a
    getChildren :: a -> [Child a]
    setChildren :: [Child a] -> a -> a

pattern Children :: HasChildrenProp a => [Child a] -> a -> a
pattern Children cs a <- (getChildren &&& id -> (cs,a)) where
    Children cs a = setChildren cs a

infixl 1 !
(!) c cs = Children cs c
       
instance HasChildrenProp (View ms) where
    type Child (View ms) = View ms

    -- Note: keyed nodes are extracted without their index
    getChildren v =
        case v of
            HTMLView _ _ _ cs    -> cs
            KHTMLView _ _ _ ks _ -> map snd ks
            SVGView _ _ _ cs     -> cs
            KSVGView _ _ _ ks _  -> map snd ks
            _                    -> []

    -- Note: keyed nodes are injected with (zip [0..])
    setChildren cs v =
        case v of
            HTMLView e t fs _    -> HTMLView e t fs cs
            KHTMLView _ t fs _ m -> list (mkHTML t) fs (zip [0..] cs)
            SVGView e t fs _     -> SVGView e t fs cs
            KSVGView _ t fs _ m  -> list (mkSVG t) fs (zip [0..] cs)
            _                    -> v

