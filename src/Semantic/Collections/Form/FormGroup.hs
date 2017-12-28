module Semantic.Collections.Form.FormGroup where

import GHC.Generics as G
import Pure.View hiding (disabled,inline,widths)
import qualified Pure.View as HTML

import Semantic.Utils

data FormGroup ms = FormGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , grouped :: Bool
    , inline :: Bool
    , unstackable :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (FormGroup ms) where
    def = (G.to gdef) { as = Div }

pattern FormGroup :: Typeable ms => FormGroup ms -> View ms
pattern FormGroup fg = View fg

instance Typeable ms => Pure FormGroup ms where
    render FormGroup_ {..} =
        let
            cs =
                ( grouped # "grouped"
                : inline # "inline"
                : unstackable # "unstackable"
                : widthProp widths def True
                : "fields"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
