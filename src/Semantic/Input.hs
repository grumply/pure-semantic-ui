module Semantic.Input
  ( module Properties
  , module Tools
  , Input(..), pattern Input
  ) where

import GHC.Generics as G
import Pure.View as View hiding (disabled,focused,transparent,Disabled,Button,Label,Input,Type)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Button
import Semantic.Icon
import Semantic.Label

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Disabled, Disabled(..)
  , pattern Error, Error(..)
  , pattern Fluid, Fluid(..)
  , pattern Focus, Focus(..)
  , pattern Focused, Focused(..)
  , pattern Inverted, Inverted(..)
  , pattern Loading, Loading(..)
  , pattern OnChange, OnChange(..)
  , pattern Size, Size(..)
  , pattern TabIndex, TabIndex(..)
  , pattern Transparent, Transparent(..)
  , pattern Type, Type(..)
  )

import Prelude hiding (error)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Input ms = Input_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , disabled :: Bool
    , error :: Bool
    , fluid :: Bool
    , focus :: Bool
    , focused :: Bool
    , inverted :: Bool
    , loading :: Bool
    , onChange :: Txt -> Ef ms IO ()
    , size :: Txt
    , tabIndex :: Maybe Int
    , transparent :: Bool
    , _type :: Txt
    } deriving (Generic)

instance Default (Input ms) where
    def = (G.to gdef) { as = Div, _type = "text" }

pattern Input :: Input ms -> View ms
pattern Input i = View i

data InputFormatter = IF
  { inputSeen :: Bool
  , labelPosition :: Maybe Txt
  , iconPosition :: Maybe Txt
  , actionPosition :: Maybe Txt
  } deriving (Generic,Default)

calculatePositions :: forall ms.  [View ms] -> InputFormatter
calculatePositions = foldl' analyze def
    where
        analyze :: InputFormatter -> View ms -> InputFormatter
        analyze IF {..} v = let nis = not inputSeen # "left" in
            case v of
                HTML.Input _ _  -> IF { inputSeen      = True    , .. }
                View Label_{}   -> IF { labelPosition  = Just nis, .. }
                View Icon_{}    -> IF { iconPosition   = Just nis, .. }
                View Button_{}  -> IF { actionPosition = Just nis, .. }
                _               -> IF {..}

instance Pure Input ms where
    render Input_ {..} =
        let
            _focus e = do
                focusNode e
                return Nothing

            addInputProps :: View ms -> View ms
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
                ( "ui"
                : size
                : disabled # "disabled"
                : error # "error"
                : fluid # "fluid"
                : focus # "focus"
                : inverted # "inverted"
                : loading # "loading"
                : transparent # "transparent"
                : may (<>> "action")  actionPosition
                : may (<>> "icon")    iconPosition
                : may (<>> "labeled") labelPosition
                : "input"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : otherAttrs
                )
                ( map addInputProps children )

instance HasProp As (Input ms) where
    type Prop As (Input ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasProp Attributes (Input ms) where
    type Prop Attributes (Input ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs i = i { attributes = cs }

instance HasProp OnChange (Input ms) where
    type Prop OnChange (Input ms) = Txt -> Ef ms IO ()
    getProp _ = onChange
    setProp _ oc i = i { onChange = oc }

instance HasProp Children (Input ms) where
    type Prop Children (Input ms) = [View ms]
    getProp _ = children
    setProp _ cs i = i { children = cs }

instance HasProp Classes (Input ms) where
    type Prop Classes (Input ms) = [Txt]
    getProp _ = classes
    setProp _ cs i = i { classes = cs }

instance HasProp Disabled (Input ms) where
    type Prop Disabled (Input ms) = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Error (Input ms) where
    type Prop Error (Input ms) = Bool
    getProp _ = error
    setProp _ e i = i { error = e }

instance HasProp Fluid (Input ms) where
    type Prop Fluid (Input ms) = Bool
    getProp _ = fluid
    setProp _ f i = i { fluid = f }

instance HasProp Focus (Input ms) where
    type Prop Focus (Input ms) = Bool
    getProp _ = focus
    setProp _ f i = i { focus = f }

instance HasProp Focused (Input ms) where
    type Prop Focused (Input ms) = Bool
    getProp _ = focused
    setProp _ f i = i { focused = f }

instance HasProp Inverted (Input ms) where
    type Prop Inverted (Input ms) = Bool
    getProp _ = inverted
    setProp _ inv i = i { inverted = inv }

instance HasProp Loading (Input ms) where
    type Prop Loading (Input ms) = Bool
    getProp _ = loading
    setProp _ l i = i { loading = l }

instance HasProp Size (Input ms) where
    type Prop Size (Input ms) = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

instance HasProp TabIndex (Input ms) where
    type Prop TabIndex (Input ms) = Maybe Int
    getProp _ = tabIndex
    setProp _ ti i = i { tabIndex = ti }

instance HasProp Transparent (Input ms) where
    type Prop Transparent (Input ms) = Bool
    getProp _ = transparent
    setProp _ t i = i { transparent = t }

instance HasProp Type (Input ms) where
    type Prop Type (Input ms) = Txt
    getProp _ = _type
    setProp _ t i = i { _type = t }
