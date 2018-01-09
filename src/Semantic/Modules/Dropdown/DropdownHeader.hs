module Semantic.Modules.Dropdown.DropdownHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data DropdownHeader ms = DropdownHeader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (DropdownHeader ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownHeader :: DropdownHeader ms -> View ms
pattern DropdownHeader dh = View dh

instance Pure DropdownHeader ms where
    render DropdownHeader_ {..} =
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
