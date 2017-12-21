module Semantic.Elements.Label.LabelDetail where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

data LabelDetail ms = LabelDetail_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (LabelDetail ms) where
    def = G.to gdef

pattern LabelDetail :: Typeable ms => LabelDetail ms -> View ms
pattern LabelDetail ld = View ld

instance Typeable ms => Pure LabelDetail ms where
    render LabelDetail_ {..} =
        as 
            ( ClassList ("detail" : classes)
            : attributes
            )
            children
            