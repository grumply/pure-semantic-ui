module Semantic.Collections.Table.TableHeaderCell where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Sorted

import Semantic.Properties.TableCell

data TableHeaderCell ms = TableHeaderCell_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , collapsing :: Bool
    , disabled :: Bool
    , error :: Bool
    , negative :: Bool
    , positive :: Bool
    , selectable :: Bool
    , singleLine :: Bool
    , sorted :: Txt
    , textAlign :: Txt
    , verticalAlign :: Txt
    , warning :: Bool
    , width :: Width
    } deriving (Generic)

instance Default (TableHeaderCell ms) where
    def = (G.to gdef) { as = Th }

pattern TableHeaderCell :: Typeable ms => TableHeaderCell ms -> View ms
pattern TableHeaderCell thc = View thc 

instance Typeable ms => Pure TableHeaderCell ms where
    render TableHeaderCell_ {..} =
        let
            cs =
                ( active # "active"
                : collapsing # "collapsing"
                : disabled # "disabled"
                : error # "error"
                : negative # "negative"
                : positive # "positive"
                : selectable # "selectable"
                : singleLine # "single line"
                : warning # "warning"
                : textAlign
                : verticalAlign
                : widthProp width "wide" def
                : sorted # (sorted <>> "sorted")
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (TableHeaderCell ms) where
    type AsProp (TableHeaderCell ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a thc = thc { as = a }

instance HasAttributesProp (TableHeaderCell ms) where
    type Attribute (TableHeaderCell ms) = Feature ms
    getAttributes = attributes
    setAttributes as thc = thc { attributes = as }

instance HasChildrenProp (TableHeaderCell ms) where
    type Child (TableHeaderCell ms) = View ms
    getChildren = children
    setChildren cs thc = thc { children = cs }

instance HasClassesProp (TableHeaderCell ms) where
    getClasses = classes
    setClasses cs thc = thc { classes = cs }

    