module Semantic.Elements.Label.LabelGroup where

import GHC.Generics as G
import Pure.View as View hiding (color)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasCircularProp(..), pattern Circular
  , HasClassesProp(..), pattern Classes
  , HasColorProp(..), pattern Color
  , HasSizeProp(..), pattern Size
  , HasTagProp(..), pattern Tag
  )

data LabelGroup ms = LabelGroup_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (LabelGroup ms) where
    def = (G.to gdef) { as = Div }

pattern LabelGroup :: LabelGroup ms -> View ms
pattern LabelGroup lg = View lg

instance Pure LabelGroup ms where
    render LabelGroup_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : circular # "circular" 
                : tag # "tag"
                : "labels"
                : classes
                )
        in
            as 
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (LabelGroup ms) where
    type AsProp (LabelGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f lg = lg { as = f }

instance HasAttributesProp (LabelGroup ms) where
    type Attribute (LabelGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs lg = lg { attributes = cs }

instance HasChildrenProp (LabelGroup ms) where
    type Child (LabelGroup ms) = View ms
    getChildren = children
    setChildren cs lg = lg { children = cs } 

instance HasCircularProp (LabelGroup ms) where
    getCircular = circular
    setCircular c lg = lg { circular = c }

instance HasClassesProp (LabelGroup ms) where
    getClasses = classes
    setClasses cs lg = lg { classes = cs }

instance HasColorProp (LabelGroup ms) where
    getColor = color
    setColor c lg = lg { color = c }

instance HasSizeProp (LabelGroup ms) where
    getSize = size
    setSize s lg = lg { size = s }

instance HasTagProp (LabelGroup ms) where
    getTag = tag
    setTag t lg = lg { tag = t }
