module Semantic.Views.Card.CardMeta where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.TextAlign

data CardMeta ms = CardMeta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardMeta ms) where
    def = (G.to gdef) { as = Div }

pattern CardMeta :: Typeable ms => CardMeta ms -> View ms
pattern CardMeta cm = View cm

instance Typeable ms => Pure CardMeta ms where
    render CardMeta_ {..} =
        let
            cs =
                ( textAlign
                : "meta"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (CardMeta ms) where
    type AsProp (CardMeta ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cm = cm { as = a }

instance HasAttributesProp (CardMeta ms) where
    type Attribute (CardMeta ms) = Feature ms
    getAttributes = attributes
    setAttributes as cm = cm { attributes = as }

instance HasChildrenProp (CardMeta ms) where
    type Child (CardMeta ms) = View ms
    getChildren = children
    setChildren cs cm = cm { children = cs }

instance HasClassesProp (CardMeta ms) where
    getClasses = classes
    setClasses cs cm = cm { classes = cs }

instance HasTextAlignProp (CardMeta ms) where
    getTextAlign = textAlign
    setTextAlign ta cm = cm { textAlign = ta }