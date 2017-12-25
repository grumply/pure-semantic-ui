module Semantic.Elements.List.ListDescription where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data ListDescription ms = ListDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ListDescription ms) where
    def = (G.to gdef) { as = Div }

pattern ListDescription :: Typeable ms => ListDescription ms -> View ms
pattern ListDescription ld = View ld

instance Typeable ms => Pure ListDescription ms where
    render ListDescription_ {..} =
        as ( ClassList (classes ++ [ "description" ]) : attributes ) children