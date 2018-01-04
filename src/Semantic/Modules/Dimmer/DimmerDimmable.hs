module Semantic.Modules.Dimmer.DimmerDimmable where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data DimmerDimmable ms = DimmerDimmable_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , blurring :: Bool
    , dimmed :: Bool
    } deriving (Generic)

instance Default (DimmerDimmable ms) where
    def = (G.to gdef) { as = Div }

pattern DimmerDimmable :: Typeable ms => DimmerDimmable ms -> View ms
pattern DimmerDimmable dd = View dd

instance Typeable ms => Pure DimmerDimmable ms where
    render DimmerDimmable_ {..} =
        let
            cs =
                ( blurring # "blurring"
                : dimmed # "dimmed"
                : "dimmable"
                : classes
                )
        in
            as 
                ( ClassList cs
                : attributes
                )
                children
