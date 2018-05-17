module Semantic.Input
  ( module Properties
  , module Tools
  , Input(..), pattern Input
  ) where

import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML

import Semantic.Utils

import Semantic.Button
import Semantic.Icon
import Semantic.Label

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Disabled, Disabled(..)
  , pattern Error, Error(..)
  , pattern Fluid, Fluid(..)
  , pattern Focus, Focus(..)
  , pattern Focused, Focused(..)
  , pattern Inverted, Inverted(..)
  , pattern Loading, Loading(..)
  , pattern Size, Size(..)
  , pattern TabIndex, TabIndex(..)
  , pattern Transparent, Transparent(..)
  , pattern Type, Type(..)
  )

import Prelude hiding (error)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Input = Input_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , disabled :: Bool
    , error :: Bool
    , fluid :: Bool
    , focus :: Bool
    , focused :: Bool
    , inverted :: Bool
    , loading :: Bool
    , size :: Txt
    , tabIndex :: Maybe Int
    , transparent :: Bool
    , _type :: Txt
    } deriving (Generic)

instance Default Input where
    def = (G.to gdef) { as = \fs cs -> Div & Features fss & Children cs, _type = "text" }

pattern Input :: Input -> Input
pattern Input i = i

data InputFormatter = IF
  { inputSeen :: Bool
  , labelPosition :: Maybe Txt
  , iconPosition :: Maybe Txt
  , actionPosition :: Maybe Txt
  } deriving (Generic,Default)

calculatePositions :: forall.  [View] -> InputFormatter
calculatePositions = foldl' analyze def
    where
        analyze :: InputFormatter -> View -> InputFormatter
        analyze IF {..} v = let nis = not inputSeen # "left" in
            case v of
                HTML.Input _ _  -> IF { inputSeen      = True    , .. }
                View Label_{}   -> IF { labelPosition  = Just nis, .. }
                View Icon_{}    -> IF { iconPosition   = Just nis, .. }
                View Button_{}  -> IF { actionPosition = Just nis, .. }
                _               -> IF {..}

instance Pure Input where
    view Input_ {..} =
        let
            _focus e = do
                focusNode e
                return Nothing

            addInputProps :: View -> View
            addInputProps (HTML.Input fs cs) =
                HTML.Input
                    (( HostRef ((focused #) . _focus)
                    : HTML.Disabled disabled
                    : HTML.Type _type
                    : index
                    : onInput onChange
                    : inputAttrs
                    ) ++ fs)
                    cs

            addInputProps c = c

            (inputAttrs,otherAttrs) = extractInputAttrs attributes

            index = maybe (disabled # Tabindex (-1)) Tabindex tabIndex

            IF {..} = calculatePositions children

            cs =
                [ "ui"
                , size
                , disabled # "disabled"
                , error # "error"
                , fluid # "fluid"
                , focus # "focus"
                , inverted # "inverted"
                , loading # "loading"
                , transparent # "transparent"
                , may (<>> "action")  actionPosition
                , may (<>> "icon")    iconPosition
                , may (<>> "labeled") labelPosition
                , "input"
                ]
        in
            as
                )
                ( map addInputProps children )

instance HasProp As Input where
    type Prop As Input = Features -> [View] -> View
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasFeatures Input where
    getFeatures = features
    setFeatures cs i = i { features = cs }

instance HasChildren Input where
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasProp Disabled Input where
    type Prop Disabled Input = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Error Input where
    type Prop Error Input = Bool
    getProp _ = error
    setProp _ e i = i { error = e }

instance HasProp Fluid Input where
    type Prop Fluid Input = Bool
    getProp _ = fluid
    setProp _ f i = i { fluid = f }

instance HasProp Focus Input where
    type Prop Focus Input = Bool
    getProp _ = focus
    setProp _ f i = i { focus = f }

instance HasProp Focused Input where
    type Prop Focused Input = Bool
    getProp _ = focused
    setProp _ f i = i { focused = f }

instance HasProp Inverted Input where
    type Prop Inverted Input = Bool
    getProp _ = inverted
    setProp _ inv i = i { inverted = inv }

instance HasProp Loading Input where
    type Prop Loading Input = Bool
    getProp _ = loading
    setProp _ l i = i { loading = l }

instance HasProp Size Input where
    type Prop Size Input = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

instance HasProp TabIndex Input where
    type Prop TabIndex Input = Maybe Int
    getProp _ = tabIndex
    setProp _ ti i = i { tabIndex = ti }

instance HasProp Transparent Input where
    type Prop Transparent Input = Bool
    getProp _ = transparent
    setProp _ t i = i { transparent = t }

instance HasProp Type Input where
    type Prop Type Input = Txt
    getProp _ = _type
    setProp _ t i = i { _type = t }
