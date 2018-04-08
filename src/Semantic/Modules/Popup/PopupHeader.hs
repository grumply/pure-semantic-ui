module Semantic.Modules.Popup.PopupHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data PopupHeader ms = PopupHeader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (PopupHeader ms) where
    def = (G.to gdef) { as = Div }

pattern PopupHeader :: PopupHeader ms -> View ms
pattern PopupHeader ph = View ph

instance Pure PopupHeader ms where
    render PopupHeader_ {..} =
        let
            cs = 
                ( "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (PopupHeader ms) where
    type AsProp (PopupHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ph = ph { as = f }

instance HasAttributesProp (PopupHeader ms) where
    type Attribute (PopupHeader ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ph = ph { attributes = cs }

instance HasChildrenProp (PopupHeader ms) where
    type Child (PopupHeader ms) = View ms
    getChildren = children
    setChildren cs ph = ph { children = cs }

instance HasClassesProp (PopupHeader ms) where
    getClasses = classes
    setClasses cs ph = ph { classes = cs }
