module Semantic.Modules.Dropdown.DropdownMenu where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data DropdownMenu ms = DropdownMenu_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    , scrolling :: Bool
    } deriving (Generic)

instance Default (DropdownMenu ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownMenu :: DropdownMenu ms -> View ms
pattern DropdownMenu dm = View dm

instance Pure DropdownMenu ms where
    render DropdownMenu_ {..} =
        let
            cs = 
                ( scrolling # "scrolling"
                : "menu transition"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children
