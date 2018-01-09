module Semantic.Modules.Popup.PopupHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data PopupHeader ms = PopupHeader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (PopupHeader ms) where
    def = (G.to gdef) { as = Div }

pattern PopupHeader :: PopupHeader ms -> View ms
pattern PopupHeader ph = View ph

instance Pure PopupHeader ms where
    render PopupHeader_ {..} =
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
