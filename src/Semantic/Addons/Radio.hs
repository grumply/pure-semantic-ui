module Semantic.Addons.Radio where

import GHC.Generics as G
import Pure.View hiding (Type)

import Semantic.Utils

import Semantic.Modules.Checkbox

data Radio ms = Radio_
    { slider :: Bool
    , toggle :: Bool
    , _type  :: Txt
    , withCheckbox :: Checkbox ms -> Checkbox ms
    } deriving (Generic)

instance Default (Radio ms) where
    def = (G.to gdef) 
        { _type = "radio"
        , withCheckbox = id 
        }

pattern Radio :: Typeable ms => Radio ms -> View ms
pattern Radio r = View r

instance Typeable ms => Pure Radio ms where
    render Radio_ {..} =
        Checkbox $ withCheckbox $ def 
            & (slider ? Slider $ id)
            & (toggle ? Toggle $ id)
            & Type _type
