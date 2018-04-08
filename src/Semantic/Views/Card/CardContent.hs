module Semantic.Views.Card.CardContent where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasExtraProp(..), pattern Extra
  , HasTextAlignProp(..), pattern TextAlign
  )

data CardContent ms = CardContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , extra :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardContent ms) where
    def = (G.to gdef) { as = Div }

pattern CardContent :: CardContent ms -> View ms
pattern CardContent cc = View cc

instance Pure CardContent ms where
    render CardContent_ {..} =
        let
            cs =
                ( extra # "extra"
                : textAlign
                : "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CardContent ms) where
    type AsProp (CardContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cc = cc { as = a }

instance HasAttributesProp (CardContent ms) where
    type Attribute (CardContent ms) = Feature ms
    getAttributes = attributes
    setAttributes as cc = cc { attributes = as }

instance HasChildrenProp (CardContent ms) where
    type Child (CardContent ms) = View ms
    getChildren = children
    setChildren cs cc = cc { children = cs }

instance HasClassesProp (CardContent ms) where
    getClasses = classes
    setClasses cs cc = cc { classes = cs }

instance HasExtraProp (CardContent ms) where
    getExtra = extra
    setExtra e cc = cc { extra = e }

instance HasTextAlignProp (CardContent ms) where
    getTextAlign = textAlign
    setTextAlign ta cc = cc { textAlign = ta }
