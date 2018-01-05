module Semantic.Addons.Radio where

import GHC.Generics as G
import Pure.View hiding (Type)

import Semantic.Utils

import Semantic.Modules.Checkbox

import Semantic.Properties.Slider
import Semantic.Properties.Toggle
import Semantic.Properties.Type
import Semantic.Properties.WithCheckbox

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

instance HasSliderProp (Radio ms) where
    getSlider = slider
    setSlider s r = r { slider = s }

instance HasToggleProp (Radio ms) where
    getToggle = toggle
    setToggle t r = r { toggle = t }

instance HasTypeProp (Radio ms) where
    getType = _type
    setType t r = r { _type = t }

instance HasWithCheckboxProp (Radio ms) where
    type WithCheckboxProp (Radio ms) = Checkbox ms -> Checkbox ms
    getWithCheckbox = withCheckbox
    setWithCheckbox wc r = r { withCheckbox = wc }