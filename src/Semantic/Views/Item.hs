module Semantic.Views.Item (module Semantic.Views.Item, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Typeable ms => Item ms -> View ms
pattern Item i = View i

instance Typeable ms => Pure Item ms where
    render Item_ {..} =
        let
            cs =
                ( "item"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
