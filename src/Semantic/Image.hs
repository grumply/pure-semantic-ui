module Semantic.Image
  ( module Properties
  , module Tools
  , Image(..), pattern Image
  , Group(..), pattern Group
  ) where

import GHC.Generics as G
import Pure.View as View hiding (disabled,hidden,inline,verticalAlign)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Avatar, Avatar(..)
  , pattern Bordered, Bordered(..)
  , pattern Centered, Centered(..)
  , pattern Children, Children(..)
  , pattern Circular, Circular(..)
  , pattern Classes, Classes(..)
  , pattern Disabled, Disabled(..)
  , pattern Floated, Floated(..)
  , pattern Fluid, Fluid(..)
  , pattern Hidden, Hidden(..)
  , pattern Inline, Inline(..)
  , pattern Rounded, Rounded(..)
  , pattern Size, Size(..)
  , pattern Spaced, Spaced(..)
  , pattern UI, UI(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Wrapped, Wrapped(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Image ms = Image_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , avatar :: Bool
    , bordered :: Bool
    , centered :: Bool
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , disabled :: Bool
    , floated :: Txt
    , fluid :: Bool
    , hidden :: Bool
    , inline :: Bool
    , rounded :: Bool
    , size :: Txt
    , spaced :: Maybe Txt
    , ui :: Bool
    , verticalAlign :: Txt
    , wrapped :: Bool
    } deriving (Generic)

instance Default (Image ms) where
    def = (G.to gdef) { ui = True, as = Img }

pattern Image :: Image ms -> View ms
pattern Image i = View i

instance Pure Image ms where
    render Image_ {..} =
        let
            cs =
                ( ui # "ui"
                : size
                : avatar # "avatar"
                : bordered # "bordered"
                : circular # "circular"
                : centered # "centered"
                : disabled # "disabled"
                : fluid # "fluid"
                : hidden # "hidden"
                : inline # "inline"
                : rounded # "rounded"
                : useKeyOrValueAndKey spaced "spaced"
                : floated # ("floated" <<>> floated)
                : verticalAlign # ("aligned" <<>> verticalAlign)
                : "image"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp Avatar (Image ms) where
    type Prop Avatar (Image ms) = Bool
    getProp _ = avatar
    setProp _ a i = i { avatar = a }

instance HasProp As (Image ms) where
    type Prop As (Image ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasProp Attributes (Image ms) where
    type Prop Attributes (Image ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs i = i { attributes = cs }

instance HasProp Bordered (Image ms) where
    type Prop Bordered (Image ms) = Bool
    getProp _ = bordered
    setProp _ b i = i { bordered = b }

instance HasProp Centered (Image ms) where
    type Prop Centered (Image ms) = Bool
    getProp _ = centered
    setProp _ c i = i { centered = c }

instance HasProp Children (Image ms) where
    type Prop Children (Image ms) = [View ms]
    getProp _ = children
    setProp _ cs i = i { children = cs }

instance HasProp Circular (Image ms) where
    type Prop Circular (Image ms) = Bool
    getProp _ = circular
    setProp _ c i = i { circular = c }

instance HasProp Classes (Image ms) where
    type Prop Classes (Image ms) = [Txt]
    getProp _ = classes
    setProp _ cs i = i { classes = cs }

instance HasProp Disabled (Image ms) where
    type Prop Disabled (Image ms) = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Floated (Image ms) where
    type Prop Floated (Image ms) = Txt
    getProp _ = floated
    setProp _ f i = i { floated = f }

instance HasProp Fluid (Image ms) where
    type Prop Fluid (Image ms) = Bool
    getProp _ = fluid
    setProp _ f i = i { fluid = f }

instance HasProp Inline (Image ms) where
    type Prop Inline (Image ms) = Bool
    getProp _ = inline
    setProp _ inl i = i { inline = inl }

instance HasProp Hidden (Image ms) where
    type Prop Hidden (Image ms) = Bool
    getProp _ = hidden
    setProp _ h i = i { hidden = h }

instance HasProp Rounded (Image ms) where
    type Prop Rounded (Image ms) = Bool
    getProp _ = rounded
    setProp _ r i = i { rounded = r }

instance HasProp Size (Image ms) where
    type Prop Size (Image ms) = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

instance HasProp Spaced (Image ms) where
    type Prop Spaced (Image ms) = Maybe Txt
    getProp _ = spaced
    setProp _ s i = i { spaced = s }

instance HasProp UI (Image ms) where
    type Prop UI (Image ms) = Bool
    getProp _ = ui
    setProp _ x i = i { ui = x }

instance HasProp VerticalAlign (Image ms) where
    type Prop VerticalAlign (Image ms) = Txt
    getProp _ = verticalAlign
    setProp _ va i = i { verticalAlign = va }

instance HasProp Wrapped (Image ms) where
    type Prop Wrapped (Image ms) = Bool
    getProp _ = wrapped
    setProp _ w i = i { wrapped = w }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group ig = View ig

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : size
                : classes
                ) ++ [ "images" ]
        in
            as (mergeClasses $ ClassList cs : attributes) children

instance HasProp As (Group ms) where
    type Prop As (Group ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f ig = ig { as = f }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs ig = ig { attributes = cs }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs ig = ig { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs ig = ig { classes = cs }

instance HasProp Size (Group ms) where
    type Prop Size (Group ms) = Txt
    getProp _ = size
    setProp _ s ig = ig { size = s }
