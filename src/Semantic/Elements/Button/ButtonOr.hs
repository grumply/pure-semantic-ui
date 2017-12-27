module Semantic.Elements.Button.ButtonOr where

import GHC.Generics as G
import Pure.View hiding (Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data ButtonOr ms = ButtonOr_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , localize :: Txt
    } deriving (Generic)

instance Default (ButtonOr ms) where
    def = (G.to gdef) { as = Div }

pattern ButtonOr :: Typeable ms => ButtonOr ms -> View ms
pattern ButtonOr bo = View bo

instance Typeable ms => Pure ButtonOr ms where
    render ButtonOr_ {..} =
        as
            ( ClassList ( "or" : classes )
            : localize # Attr "data-text" localize
            : attributes
            )
            []

instance HasAs (ButtonOr ms) where
    type Constructor (ButtonOr ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f bo = bo { as = f }

instance HasAttributes (ButtonOr ms) where
    type Attribute (ButtonOr ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs bo = bo { attributes = cs }

instance HasClasses (ButtonOr ms) where
    getClasses = classes
    setClasses cs bo = bo { classes = cs }