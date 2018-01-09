module Semantic.Modules.Dropdown.DropdownDivider where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data DropdownDivider ms = DropdownDivider_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , classes :: [Txt]
    } deriving (Generic)

instance Default (DropdownDivider ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownDivider :: DropdownDivider ms -> View ms
pattern DropdownDivider dd = View dd

instance Pure DropdownDivider ms where
    render DropdownDivider_ {..} =
        let
            cs = 
                ( "divider"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                []
