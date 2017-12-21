module Semantic.Elements.Image.ImageGroup where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

data ImageGroup ms = ImageGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (ImageGroup ms) where
    def = (G.to gdef) { as = Div }

pattern ImageGroup :: Typeable ms => ImageGroup ms -> View ms
pattern ImageGroup ig = View ig

instance Typeable ms => Pure ImageGroup ms where
    render ImageGroup_ {..} =
        let
            cs =
                ( "ui"
                : size
                : classes
                ) ++ [ "images" ]
        in 
            as (ClassList cs : attributes) children