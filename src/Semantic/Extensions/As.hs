module Semantic.Extensions.As where

import Pure.View

import Semantic.Extensions.Utils
    
class HasAs a where
    type Constructor a
    getAs :: a -> Constructor a
    setAs :: Constructor a -> a -> a

pattern As :: HasAs a => Constructor a -> a -> a
pattern As as a <- (getAs &&& id -> (as,a)) where
    As as a = setAs as a
       
instance HasAs (View ms) where
    type Constructor (View ms) = [Feature ms] -> [View ms] -> View ms

    -- Note: For Managed, Component, Text, Null, and Raw views, this 
    -- method just extracts a reinjector that ignores its arguments.
    -- For keyed nodes, KHTML and KSVG, this method will create a 
    -- `Constructor (View ms)` that adds indexes with (zip [0..]).
    getAs v =
        case v of
            HTMLView _ t _ _    -> mkHTML t
            KHTMLView _ t _ _ _ -> \fs cs -> list (mkHTML t) fs (zip [0..] cs)
            SVGView _ t _ _     -> mkSVG t
            KSVGView _ t _ _ _  -> \fs cs -> list (mkSVG t) fs (zip [0..] cs)
            v                   -> (\_ _ -> v)

    -- Note: For Managed, Coponent, Text, Null, and Raw views, this
    -- method ignores its `as` argument and returns the view as is.
    setAs as v =
        case v of
            HTMLView _ _ fs cs    -> as fs cs
            KHTMLView _ _ fs cs _ -> list as fs cs
            SVGView _ _ fs cs     -> as fs cs
            KSVGView _ _ fs cs _  -> list as fs cs
            _                     -> v
    