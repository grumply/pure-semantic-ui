module Semantic.Modules.Modal.ModalHeader where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ModalHeader ms = ModalHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ModalHeader ms) where
    def = (G.to gdef) { as = Div }

pattern ModalHeader :: Typeable ms => ModalHeader ms -> View ms
pattern ModalHeader mh = View mh

instance Typeable ms => Pure ModalHeader ms where
    render ModalHeader_ {..} =
        let
            cs = classes <> [ "header" ]

        in
            as
                ( ClassList cs
                : attributes
                ) 
                children

instance HasAsProp (ModalHeader ms) where
    type AsProp (ModalHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f mh = mh { as = f }

instance HasAttributesProp (ModalHeader ms) where
    type Attribute (ModalHeader ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs mh = mh { attributes = cs }

instance HasChildrenProp (ModalHeader ms) where
    type Child (ModalHeader ms) = View ms
    getChildren = children
    setChildren cs mh = mh { children = cs }

instance HasClassesProp (ModalHeader ms) where
    getClasses = classes
    setClasses cs mh = mh { classes = cs }