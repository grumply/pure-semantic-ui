module Semantic.Collections.Table.TableFooter where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data TableFooter ms = TableFooter_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (TableFooter ms) where
    def = (G.to gdef) { as = Tfoot }

pattern TableFooter :: Typeable ms => TableFooter ms -> View ms
pattern TableFooter tf = View tf 

instance Typeable ms => Pure TableFooter ms where
    render TableFooter_ {..} =
        as
            ( ClassList classes
            : attributes
            )
            children
