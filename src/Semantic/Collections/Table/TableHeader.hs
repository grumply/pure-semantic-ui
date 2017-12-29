module Semantic.Collections.Table.TableHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data TableHeader ms = TableHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fullWidth :: Bool
    } deriving (Generic)

instance Default (TableHeader ms) where
    def = (G.to gdef) { as = Thead }

pattern TableHeader :: Typeable ms => TableHeader ms -> View ms
pattern TableHeader th = View th

instance Typeable ms => Pure TableHeader ms where
    render TableHeader_ {..} =
        let
            cs =
                ( fullWidth # "full-width"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (TableHeader ms) where
    type AsProp (TableHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a th = th { as = a }

instance HasAttributesProp (TableHeader ms) where
    type Attribute (TableHeader ms) = Feature ms
    getAttributes = attributes
    setAttributes as th = th { attributes = as }

instance HasChildrenProp (TableHeader ms) where
    type Child (TableHeader ms) = View ms
    getChildren = children
    setChildren cs th = th { children = cs }

instance HasClassesProp (TableHeader ms) where
    getClasses = classes
    setClasses cs th = th { classes = cs }

    