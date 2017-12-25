module Semantic.Elements.List.ListContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data ListContent ms = ListContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , floated :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (ListContent ms) where
    def = (G.to gdef) { as = Div }

pattern ListContent :: Typeable ms => ListContent ms -> View ms
pattern ListContent lc = View lc

instance Typeable ms => Pure ListContent ms where
    render ListContent_ {..} =
        let
            cs =
                ( floated # (floated <>> "floated")
                : verticalAlign # (verticalAlign <>> "aligned")
                : "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children