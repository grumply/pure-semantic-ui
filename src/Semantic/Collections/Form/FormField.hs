module Semantic.Collections.Form.FormField where

import GHC.Generics as G
import Pure.View hiding (name,onSubmit,widths,Form)
import qualified Pure.View as HTML

import Semantic.Utils

data FormField ms = FormField_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , disabled :: Bool
    , error :: Bool
    , inline :: Bool
    , required :: Bool
    , _type :: Txt
    , widths :: Width
    } deriving (Generic)

instance Default (FormField ms) where
    def = (G.to gdef) { as = Div }

pattern FormField :: Typeable ms => FormField ms -> View ms
pattern FormField ff = View ff

instance Typeable ms => Pure FormField ms where
    render FormField_ {..} =
        let
            cs =
                ( disabled # "disabled"
                : error # "error"
                : inline # "inline"
                : required # "required"
                : widthProp widths "wide" def
                : "field"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children