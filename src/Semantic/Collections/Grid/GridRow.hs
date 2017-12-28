module Semantic.Collections.Grid.GridRow where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data GridRow ms = GridRow_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , color :: Txt
    , columns :: Width
    , divided :: Bool
    , only :: [Txt]
    , reversed :: [Txt]
    , stretched :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (GridRow ms) where
    def = (G.to gdef) { as = Div }

pattern GridRow :: Typeable ms => GridRow ms -> View ms
pattern GridRow gr = View gr

instance Typeable ms => Pure GridRow ms where
    render GridRow_ {..} =
        let
            cs =
                ( color
                : centered # "centered"
                : divided # "divided"
                : stretched # "stretched"
                : multiProp only "only"
                : multiProp reversed "reversed"
                : textAlign
                : verticalAlign
                : widthProp columns "columns" True
                : "row"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children