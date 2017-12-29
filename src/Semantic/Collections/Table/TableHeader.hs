module Semantic.Collections.Table.TableHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data TableHeader ms = TableHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fullWidth :: Bool
    } deriving (Generic)

instance Default (TableHeader ms) where
    def = (G.to gdef) { as = Thead }

pattern TableHeader :: Typeable ms => TableHeader ms -> View ms
pattern TableHeader tf = View tf 

instance Typeable ms => Pure TableHeader ms where
    render TableHeader_ {..} =
        let
            cs =
                ( fullWidth # "full-width"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
