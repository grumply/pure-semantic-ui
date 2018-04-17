module Semantic.Checkbox
  ( module Properties
  , module Tools
  , Checkbox(..), pattern Checkbox
  ) where

import GHC.Generics as G
import Pure.View hiding (disabled,name,onClick,Checked,Name,Type,Value)
import qualified Pure.View as HTML
import Pure.Lifted (Node)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Disabled, Disabled(..)
  , pattern Fitted, Fitted(..)
  , pattern Checked, Checked(..)
  , pattern Name, Name(..)
  , pattern OnChange, OnChange(..)
  , pattern OnClick, OnClick(..)
  , pattern OnMouseDown, OnMouseDown(..)
  , pattern WithRef, WithRef(..)
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

data Checkbox ms = Checkbox_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , checked :: Maybe Bool
    , disabled :: Bool
    , fitted :: Bool
    , name :: Txt
    , onChange :: Checkbox ms -> Ef ms IO ()
    , onClick :: Checkbox ms -> Ef ms IO ()
    , onMouseDown :: Checkbox ms -> Ef ms IO ()
    , withRef :: Node -> Ef ms IO ()
    , radio :: Bool
    , readOnly :: Bool
    , slider :: Bool
    , tabIndex :: Maybe Int
    , toggle :: Bool
    , _type :: Txt
    , value :: Txt
    } deriving (Generic)

instance Default (Checkbox ms) where
    def = (G.to gdef) { as = Div }

pattern Checkbox :: Checkbox ms -> View ms
pattern Checkbox c = View c

pattern Radio :: Checkbox ms -> View ms
pattern Radio r = View (Type "radio" (IsRadio True r))

renderChecked Nothing = HTML.Checked False
renderChecked (Just True) = HTML.Checked True
renderChecked (Just False) = nil

instance Pure Checkbox ms where
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
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : On "change" def (\_ -> return $ Just (onChange cb))
                : On "click" def (\_ -> return $ Just (onClick cb))
                : On "mousedown" def (\_ -> return $ Just (onMouseDown cb))
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

instance HasProp As (Checkbox ms) where
    type Prop As (Checkbox ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a cb = cb { as = a }

instance HasProp Attributes (Checkbox ms) where
    type Prop Attributes (Checkbox ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as cb = cb { attributes = as }

instance HasProp Children (Checkbox ms) where
    type Prop Children (Checkbox ms) = [View ms]
    getProp _ = children
    setProp _ cs cb = cb { children = cs }

instance HasProp Classes (Checkbox ms) where
    type Prop Classes (Checkbox ms) = [Txt]
    getProp _ = classes
    setProp _ cs cb = cb { classes = cs }

instance HasProp Disabled (Checkbox ms) where
    type Prop Disabled (Checkbox ms) = Bool
    getProp _ = disabled
    setProp _ d cb = cb { disabled = d }

instance HasProp Fitted (Checkbox ms) where
    type Prop Fitted (Checkbox ms) = Bool
    getProp _ = fitted
    setProp _ f cb = cb { fitted = f }

instance HasProp Checked (Checkbox ms) where
    type Prop Checked (Checkbox ms) = Maybe Bool
    getProp _ = checked
    setProp _ c cb = cb { checked = c }

instance HasProp Name (Checkbox ms) where
    type Prop Name (Checkbox ms) = Txt
    getProp _ = name
    setProp _ n cb = cb { name = n }

instance HasProp OnChange (Checkbox ms) where
    type Prop OnChange (Checkbox ms) = Checkbox ms -> Ef ms IO ()
    getProp _ = onChange
    setProp _ oc cb = cb { onChange = oc }

instance HasProp OnClick (Checkbox ms) where
    type Prop OnClick (Checkbox ms) = Checkbox ms -> Ef ms IO ()
    getProp _ = onClick
    setProp _ oc cb = cb { onClick = oc }

instance HasProp OnMouseDown (Checkbox ms) where
    type Prop OnMouseDown (Checkbox ms) = Checkbox ms -> Ef ms IO ()
    getProp _ = onMouseDown
    setProp _ omd cb = cb { onMouseDown = omd }

instance HasProp WithRef (Checkbox ms) where
    type Prop WithRef (Checkbox ms) = Node -> Ef ms IO ()
    getProp _ = withRef
    setProp _ wr cb = cb { withRef = wr }

instance HasProp IsRadio (Checkbox ms) where
    type Prop IsRadio (Checkbox ms) = Bool
    getProp _ = radio
    setProp _ r cb = cb { radio = r }

instance HasProp ReadOnly (Checkbox ms) where
    type Prop ReadOnly (Checkbox ms) = Bool
    getProp _ = readOnly
    setProp _ ro cb = cb { readOnly = ro }

instance HasProp Slider (Checkbox ms) where
    type Prop Slider (Checkbox ms) = Bool
    getProp _ = slider
    setProp _ s cb = cb { slider = s }

instance HasProp TabIndex (Checkbox ms) where
    type Prop TabIndex (Checkbox ms) = Maybe Int
    getProp _ = tabIndex
    setProp _ ti cb = cb { tabIndex = ti }

instance HasProp Toggle (Checkbox ms) where
    type Prop Toggle (Checkbox ms) = Bool
    getProp _ = toggle
    setProp _ t cb = cb { toggle = t }

instance HasProp Type (Checkbox ms) where
    type Prop Type (Checkbox ms) = Txt
    getProp _ = _type
    setProp _ t cb = cb { _type = t }

instance HasProp Value (Checkbox ms) where
    type Prop Value (Checkbox ms) = Txt
    getProp _ = value
    setProp _ v cb = cb { value = v }

