module Semantic.Modules.Modal.ModalDescription where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ModalDescription ms = ModalDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ModalDescription ms) where
    def = (G.to gdef) { as = Div }

pattern ModalDescription :: ModalDescription ms -> View ms
pattern ModalDescription md = View md

instance Pure ModalDescription ms where
    render ModalDescription_ {..} =
        let
            cs =
                ( "description"
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                ) 
                children

instance HasAsProp (ModalDescription ms) where
    type AsProp (ModalDescription ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f md = md { as = f }

instance HasAttributesProp (ModalDescription ms) where
    type Attribute (ModalDescription ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs md = md { attributes = cs }

instance HasChildrenProp (ModalDescription ms) where
    type Child (ModalDescription ms) = View ms
    getChildren = children
    setChildren cs md = md { children = cs }

instance HasClassesProp (ModalDescription ms) where
    getClasses = classes
    setClasses cs md = md { classes = cs }