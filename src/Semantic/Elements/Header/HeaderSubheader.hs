module Semantic.Elements.Header.HeaderSubheader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data HeaderSubheader ms = HeaderSubheader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (HeaderSubheader ms) where
    def = (G.to gdef) { as = Div }

pattern HeaderSubheader :: HeaderSubheader ms -> View ms
pattern HeaderSubheader hs = View hs

instance Pure HeaderSubheader ms where
    render HeaderSubheader_ {..} =
        as
            ( ClassList ( "sub" : "header" : classes )
            : attributes
            )
            children

instance HasAsProp (HeaderSubheader ms) where
    type AsProp (HeaderSubheader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f hs = hs { as = f }

instance HasAttributesProp (HeaderSubheader ms) where
    type Attribute (HeaderSubheader ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs hs = hs { attributes = cs }

instance HasChildrenProp (HeaderSubheader ms) where
    type Child (HeaderSubheader ms) = View ms
    getChildren = children
    setChildren cs hs = hs { children = cs }

instance HasClassesProp (HeaderSubheader ms) where
    getClasses = classes
    setClasses cs hs = hs { classes = cs }
