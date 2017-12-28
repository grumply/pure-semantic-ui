module Semantic.Elements.Flag where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data Flag ms = Flag_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , name :: Txt
    } deriving (Generic)

instance Default (Flag ms) where
    def = (G.to gdef) { as = I }

pattern Flag :: Typeable ms => Flag ms -> View ms
pattern Flag f = View f

instance Typeable ms => Pure Flag ms where
    render Flag_ {..} =
        let
            cs =
                ( name
                : "flag"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                []