module Semantic.Elements.Button.ButtonContent where

import GHC.Generics as G
import Pure.View hiding (hidden,visible,Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Hidden
import Semantic.Properties.Visible

data ButtonContent ms = ButtonContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , hidden :: Bool
    , visible :: Bool
    } deriving (Generic)

instance Default (ButtonContent ms) where
    def = (G.to gdef) { as = Div }

pattern ButtonContent :: Typeable ms => ButtonContent ms -> View ms
pattern ButtonContent bc = View bc

instance Typeable ms => Pure ButtonContent ms where
    render ButtonContent_ {..} =
        let
            cs =
                ( hidden # "hidden"
                : visible # "visible"
                : "content"
                : classes
                )

        in
            as
                ( ClassList cs
                : attributes
                ) 
                children

instance HasAsProp (ButtonContent ms) where
    type AsProp (ButtonContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f bc = bc { as = f }

instance HasAttributesProp (ButtonContent ms) where
    type Attribute (ButtonContent ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs bc = bc { attributes = cs }

instance HasChildrenProp (ButtonContent ms) where
    type Child (ButtonContent ms) = View ms
    getChildren = children
    setChildren cs bc = bc { children = cs }

instance HasClassesProp (ButtonContent ms) where
    getClasses = classes
    setClasses cs bc = bc { classes = cs }

instance HasHiddenProp (ButtonContent ms) where
    getHidden = hidden
    setHidden h bc = bc { hidden = h }

instance HasVisibleProp (ButtonContent ms) where
    getVisible = visible
    setVisible v bc = bc { visible = v }