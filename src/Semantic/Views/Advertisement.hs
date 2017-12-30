module Semantic.Views.Advertisement where

import GHC.Generics as G
import Pure.View hiding (unit)

import Semantic.Utils

data Advertisement ms = Advertisement_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , test :: Txt
    , unit :: Txt
    } deriving (Generic)

instance Default (Advertisement ms) where
    def = (G.to gdef) { as = Div }

pattern Advertisement :: Typeable ms => Advertisement ms -> View ms
pattern Advertisement a = View a

instance Typeable ms => Pure Advertisement ms where
    render Advertisement_ {..} =
        let
            cs =
                ( "ui"
                : unit
                : centered # "centered"
                : test # "test"
                : "ad"
                : classes
                )
        in
            as
                ( ClassList cs
                : test # (Property "data-text" test)
                : attributes
                )
                children
