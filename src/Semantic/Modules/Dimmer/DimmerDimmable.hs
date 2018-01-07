module Semantic.Modules.Dimmer.DimmerDimmable where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Blurring
import Semantic.Properties.Dimmed

data DimmerDimmable ms = DimmerDimmable_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , blurring :: Bool
    , dimmed :: Bool
    } deriving (Generic)

instance Default (DimmerDimmable ms) where
    def = (G.to gdef) { as = Div }

pattern DimmerDimmable :: DimmerDimmable ms -> View ms
pattern DimmerDimmable dd = View dd

instance Pure DimmerDimmable ms where
    render DimmerDimmable_ {..} =
        let
            cs =
                ( blurring # "blurring"
                : dimmed # "dimmed"
                : "dimmable"
                : classes
                )
        in
            as 
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children
 
instance HasAsProp (DimmerDimmable ms) where
    type AsProp (DimmerDimmable ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a dd = dd { as = a }

instance HasAttributesProp (DimmerDimmable ms) where
    type Attribute (DimmerDimmable ms) = Feature ms
    getAttributes = attributes
    setAttributes as dd = dd { attributes = as }

instance HasChildrenProp (DimmerDimmable ms) where
    type Child (DimmerDimmable ms) = View ms
    getChildren = children
    setChildren cs dd = dd { children = cs }

instance HasClassesProp (DimmerDimmable ms) where
    getClasses = classes
    setClasses cs dd = dd { classes = cs }

instance HasBlurringProp (DimmerDimmable ms) where
    getBlurring = blurring
    setBlurring b dd = dd { blurring = b }

instance HasDimmedProp (DimmerDimmable ms) where
    getDimmed = dimmed
    setDimmed d dd = dd { dimmed = d }