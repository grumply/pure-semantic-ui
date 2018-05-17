module Semantic.Checkbox
  ( module Properties
  , module Tools
  , Checkbox(..), pattern Checkbox
  ) where

import GHC.Generics as G
import qualified Pure.View as HTML
import Pure.Lifted (Node)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Disabled, Disabled(..)
  , pattern Fitted, Fitted(..)
  , pattern Checked, Checked(..)
  , pattern Name, Name(..)
  , pattern IsRadio, IsRadio(..)
  , pattern ReadOnly, ReadOnly(..)
  , pattern Slider, Slider(..)
  , pattern TabIndex, TabIndex(..)
  , pattern Toggle, Toggle(..)
  , pattern Type, Type(..)
  , pattern Value, Value(..)
  , Checked(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Checkbox = Checkbox_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , checked :: Maybe Bool
    , disabled :: Bool
    , fitted :: Bool
    , name :: Txt
    , radio :: Bool
    , readOnly :: Bool
    , slider :: Bool
    , tabIndex :: Maybe Int
    , toggle :: Bool
    , _type :: Txt
    , value :: Txt
    } deriving (Generic)

instance Default Checkbox where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Checkbox :: Checkbox -> Checkbox
pattern Checkbox c = c

pattern Radio :: Checkbox -> Checkbox
pattern Radio r = (Type "radio" (IsRadio True r))

renderChecked Nothing = HTML.Checked False
renderChecked (Just True) = HTML.Checked True
renderChecked (Just False) = nil

instance Pure Checkbox where
    render cb@Checkbox_ {..} =
        let
            cs =
                ( "ui"
                : (checked == Just True) # "checked"
                : disabled # "disabled"
                : (checked == Nothing) # "indeterminate"
                : fitted # "fitted"
                : radio # "radio"
                : readOnly # "read-only"
                : slider # "slider"
                : toggle # "toggle"
                : "checkbox"
                )

        in
            as
                : attributes
                )
                ( HTML.Input
                    [ ClassList [ "hidden" ]
                    , HostRef (return . Just . withRef)
                    , renderChecked checked
                    , HTML.Name name
                    , Readonly readOnly
                    , may (\ti -> Tabindex (disabled ? (-1) $ ti)) tabIndex
                    , HTML.Type _type
                    , HTML.Value value
                    ]
                    []
                : children
                )

instance HasProp As Checkbox where
    type Prop As Checkbox = Features -> [View] -> View
    getProp _ = as
    setProp _ a cb = cb { as = a }

instance HasFeatures Checkbox where
    getFeatures = features
    setFeatures as cb = cb { features = as }

instance HasChildren Checkbox where
    getChildren = children
    setChildren cs cb = cb { children = cs }


instance HasProp Disabled Checkbox where
    type Prop Disabled Checkbox = Bool
    getProp _ = disabled
    setProp _ d cb = cb { disabled = d }

instance HasProp Fitted Checkbox where
    type Prop Fitted Checkbox = Bool
    getProp _ = fitted
    setProp _ f cb = cb { fitted = f }

instance HasProp Checked Checkbox where
    type Prop Checked Checkbox = Maybe Bool
    getProp _ = checked
    setProp _ c cb = cb { checked = c }

instance HasProp Name Checkbox where
    type Prop Name Checkbox = Txt
    getProp _ = name
    setProp _ n cb = cb { name = n }

instance HasProp IsRadio Checkbox where
    type Prop IsRadio Checkbox = Bool
    getProp _ = radio
    setProp _ r cb = cb { radio = r }

instance HasProp ReadOnly Checkbox where
    type Prop ReadOnly Checkbox = Bool
    getProp _ = readOnly
    setProp _ ro cb = cb { readOnly = ro }

instance HasProp Slider Checkbox where
    type Prop Slider Checkbox = Bool
    getProp _ = slider
    setProp _ s cb = cb { slider = s }

instance HasProp TabIndex Checkbox where
    type Prop TabIndex Checkbox = Maybe Int
    getProp _ = tabIndex
    setProp _ ti cb = cb { tabIndex = ti }

instance HasProp Toggle Checkbox where
    type Prop Toggle Checkbox = Bool
    getProp _ = toggle
    setProp _ t cb = cb { toggle = t }

instance HasProp Type Checkbox where
    type Prop Type Checkbox = Txt
    getProp _ = _type
    setProp _ t cb = cb { _type = t }

instance HasProp Value Checkbox where
    type Prop Value Checkbox = Txt
    getProp _ = value
    setProp _ v cb = cb { value = v }

