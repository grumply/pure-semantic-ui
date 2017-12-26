module Semantic.Elements.List.ListHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Extensions.Children

data ListHeader ms = ListHeader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ListHeader ms) where
    def = (G.to gdef) { as = Div }

pattern ListHeader :: Typeable ms => ListHeader ms -> View ms
pattern ListHeader lh = View lh

instance Typeable ms => Pure ListHeader ms where
    render ListHeader_ {..} =
        as ( ClassList ( "header" : classes ) : attributes ) children

instance HasChildren (ListHeader ms) where
    type Child (ListHeader ms) = View ms
    getChildren = children
    setChildren cs lh = lh { children = cs }