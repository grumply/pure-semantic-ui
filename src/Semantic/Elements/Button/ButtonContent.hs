module Semantic.Elements.Button.ButtonContent where

import GHC.Generics as G
import Pure.View hiding (Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Extensions.Children

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

instance HasChildren (ButtonContent ms) where
    type Child (ButtonContent ms) = View ms
    getChildren = children
    setChildren cs bc = bc { children = cs }