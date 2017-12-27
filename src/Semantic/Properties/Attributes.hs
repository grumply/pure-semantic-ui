module Semantic.Properties.Attributes where

import Pure.View

import Semantic.Properties.Utils

class HasAttributesProp a where
    type Attribute a
    getAttributes :: a -> [Attribute a]
    setAttributes :: [Attribute a] -> a -> a

pattern Attributes :: HasAttributesProp a => [Attribute a] -> a -> a
pattern Attributes cs a <- (getAttributes &&& id -> (cs,a)) where
    Attributes cs a = setAttributes cs a

infixl 8 %
(%) c as = Attributes as c
       
instance HasAttributesProp (View ms) where
    type Attribute (View ms) = Feature ms
    getAttributes v =
        case v of
            HTMLView _ _ fs _    -> fs
            KHTMLView _ _ fs _ _ -> fs
            SVGView _ _ fs _     -> fs
            KSVGView _ _ fs _ _  -> fs
            ManagedView _ _ fs _ -> fs
            _                    -> []
    setAttributes fs v =
        case v of
            HTMLView e t _ cs    -> HTMLView e t fs cs
            KHTMLView e t _ cs m -> KHTMLView e t fs cs m
            SVGView e t _ cs     -> SVGView e t fs cs
            KSVGView e t _ cs m  -> KSVGView e t fs cs m
            ManagedView e t _ c  -> ManagedView e t fs c
            _                    -> v
    

