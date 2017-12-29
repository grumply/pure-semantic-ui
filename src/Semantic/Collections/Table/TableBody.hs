module Semantic.Collections.Table.TableBody where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data TableBody ms = TableBody_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (TableBody ms) where
    def = (G.to gdef) { as = Tbody }

pattern TableBody :: Typeable ms => TableBody ms -> View ms
pattern TableBody tb = View tb

instance Typeable ms => Pure TableBody ms where
    render TableBody_ {..} =
        as
            ( ClassList classes
            : attributes
            )
            children
