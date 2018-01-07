module Semantic.Modules.Modal.ModalActions where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ModalActions ms = ModalActions_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ModalActions ms) where
    def = (G.to gdef) { as = Div }

pattern ModalActions :: ModalActions ms -> View ms
pattern ModalActions ma = View ma

instance Pure ModalActions ms where
    render ModalActions_ {..} =
        as
            ( ClassList ( "actions" : classes )
            : attributes
            ) 
            children

instance HasAsProp (ModalActions ms) where
    type AsProp (ModalActions ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ma = ma { as = f }

instance HasAttributesProp (ModalActions ms) where
    type Attribute (ModalActions ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ma = ma { attributes = cs }

instance HasChildrenProp (ModalActions ms) where
    type Child (ModalActions ms) = View ms
    getChildren = children
    setChildren cs ma = ma { children = cs }

instance HasClassesProp (ModalActions ms) where
    getClasses = classes
    setClasses cs ma = ma { classes = cs }