module Semantic.Views.Card.CardHeader where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.TextAlign

data CardHeader ms = CardHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardHeader ms) where
    def = (G.to gdef) { as = Div }

pattern CardHeader :: CardHeader ms -> View ms
pattern CardHeader ch = View ch 

instance Pure CardHeader ms where
    render CardHeader_ {..} =
        let
            cs =
                ( textAlign
                : "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CardHeader ms) where
    type AsProp (CardHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ch = ch { as = a }

instance HasAttributesProp (CardHeader ms) where
    type Attribute (CardHeader ms) = Feature ms
    getAttributes = attributes
    setAttributes as ch = ch { attributes = as }

instance HasChildrenProp (CardHeader ms) where
    type Child (CardHeader ms) = View ms
    getChildren = children
    setChildren cs ch = ch { children = cs }

instance HasClassesProp (CardHeader ms) where
    getClasses = classes
    setClasses cs ch = ch { classes = cs }

instance HasTextAlignProp (CardHeader ms) where
    getTextAlign = textAlign
    setTextAlign ta ch = ch { textAlign = ta }