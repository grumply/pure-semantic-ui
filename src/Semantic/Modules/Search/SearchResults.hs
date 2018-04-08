module Semantic.Modules.Search.SearchResults where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data SearchResults ms = SearchResults_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (SearchResults ms) where
    def = (G.to gdef) { as = Div }

pattern SearchResults :: SearchResults ms -> View ms
pattern SearchResults sr = View sr

instance Pure SearchResults ms where
    render SearchResults_ {..} =
        let
            cs = 
                ( "results transition"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (SearchResults ms) where
    type AsProp (SearchResults ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sr = sr { as = f }

instance HasAttributesProp (SearchResults ms) where
    type Attribute (SearchResults ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs sr = sr { attributes = cs }

instance HasChildrenProp (SearchResults ms) where
    type Child (SearchResults ms) = View ms
    getChildren = children
    setChildren cs sr = sr { children = cs }

instance HasClassesProp (SearchResults ms) where
    getClasses = classes
    setClasses cs sr = sr { classes = cs }
