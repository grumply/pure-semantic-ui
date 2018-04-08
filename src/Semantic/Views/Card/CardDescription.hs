module Semantic.Views.Card.CardDescription where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasTextAlignProp(..), pattern TextAlign
  )

data CardDescription ms = CardDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardDescription ms) where
    def = (G.to gdef) { as = Div }

pattern CardDescription :: CardDescription ms -> View ms
pattern CardDescription cd = View cd

instance Pure CardDescription ms where
    render CardDescription_ {..} =
        let
            cs =
                ( textAlign
                : "description"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CardDescription ms) where
    type AsProp (CardDescription ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cd = cd { as = a }

instance HasAttributesProp (CardDescription ms) where
    type Attribute (CardDescription ms) = Feature ms
    getAttributes = attributes
    setAttributes as cd = cd { attributes = as }

instance HasChildrenProp (CardDescription ms) where
    type Child (CardDescription ms) = View ms
    getChildren = children
    setChildren cs cd = cd { children = cs }

instance HasClassesProp (CardDescription ms) where
    getClasses = classes
    setClasses cs cd = cd { classes = cs }

instance HasTextAlignProp (CardDescription ms) where
    getTextAlign = textAlign
    setTextAlign ta cd = cd { textAlign = ta }
