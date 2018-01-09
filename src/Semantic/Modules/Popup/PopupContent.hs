module Semantic.Modules.Popup.PopupContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data PopupContent ms = PopupContent_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (PopupContent ms) where
    def = (G.to gdef) { as = Div }

pattern PopupContent :: PopupContent ms -> View ms
pattern PopupContent pc = View pc

instance Pure PopupContent ms where
    render PopupContent_ {..} =
        let
            cs = 
                ( "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children
