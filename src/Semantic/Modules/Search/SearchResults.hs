module Semantic.Modules.Search.SearchResults where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

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
