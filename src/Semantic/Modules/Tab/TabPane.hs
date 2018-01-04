module Semantic.Modules.Tab.TabPane where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

import Semantic.Elements.Segment

import Semantic.Properties.Attached

import Semantic.Properties.Attributes
import Semantic.Properties.Children
data TabPane ms = TabPane_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , loading :: Bool
    } deriving (Generic)

instance Typeable ms => Default (TabPane ms) where
    def = (G.to gdef) 
        { as = \fs cs -> 
            Segment $ def 
                & Attached "bottom" 
                & Attributes fs 
                & Children cs 
        }

pattern TabPane :: Typeable ms => TabPane ms -> View ms
pattern TabPane tp = View tp

instance Typeable ms => Pure TabPane ms where
    render TabPane_ {..} =
        let
            cs =
                ( active # "active"
                : loading # "loading"
                : "tab"
                : classes
                )
        in
            as 
                ( ClassList cs
                : attributes
                )
                children
