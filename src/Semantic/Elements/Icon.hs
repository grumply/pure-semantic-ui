module Semantic.Elements.Icon
  ( module Properties
  , module Tools
  , Icon(..), pattern Icon
  , Group(..), pattern Group
  ) where

import GHC.Generics as G
import Pure.View as View hiding (color,disabled,name)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Bordered, Bordered(..)
  , pattern Circular, Circular(..)
  , pattern Classes, Classes(..)
  , pattern Color, Color(..)
  , pattern Corner, Corner(..)
  , pattern Disabled, Disabled(..)
  , pattern Fitted, Fitted(..)
  , pattern Flipped, Flipped(..)
  , pattern Inverted, Inverted(..)
  , pattern Link, Link(..)
  , pattern Loading, Loading(..)
  , pattern Name, Name(..)
  , pattern Rotated, Rotated(..)
  , pattern Size, Size(..)
  )

data Icon ms = Icon_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , bordered :: Bool
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , corner :: Bool
    , disabled :: Bool
    , fitted :: Bool
    , flipped :: Txt
    , inverted :: Bool
    , link :: Bool
    , loading :: Bool
    , name :: Txt
    , rotated :: Txt
    , size :: Txt
    } deriving (Generic)

instance Default (Icon ms) where
    def = (G.to gdef) { as = I }

pattern Icon :: Icon ms -> View ms
pattern Icon i = View i

instance Pure Icon ms where
    render Icon_ {..} =
        let
            cs =
                ( color
                : name
                : cond size
                : bordered # "bordered"
                : circular # "circular"
                : corner # "corner"
                : disabled # "disabled"
                : fitted # "fitted"
                : inverted # "inverted"
                : link # "link"
                : loading # "loading"
                : flipped # ("flipped" <<>> flipped)
                : rotated # ("rotated" <<>> rotated)
                : "icon"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : Attr "aria-hidden" "true"
                : attributes
                )
                []

instance HasProp As (Icon ms) where
    type Prop As (Icon ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasProp Attributes (Icon ms) where
    type Prop Attributes (Icon ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs i = i { attributes = cs }

instance HasProp Bordered (Icon ms) where
    type Prop Bordered (Icon ms) = Bool
    getProp _ = bordered
    setProp _ b i = i { bordered = b }

instance HasProp Circular (Icon ms) where
    type Prop Circular (Icon ms) = Bool
    getProp _ = circular
    setProp _ c i = i { circular = c }

instance HasProp Name (Icon ms) where
    type Prop Name (Icon ms) = Txt
    getProp _ = name
    setProp _ n i = i { name = n }

instance HasProp Classes (Icon ms) where
    type Prop Classes (Icon ms) = [Txt]
    getProp _ = classes
    setProp _ cs i = i { classes = cs }

instance HasProp Color (Icon ms) where
    type Prop Color (Icon ms) = Txt
    getProp _ = color
    setProp _ c i = i { color = c }

instance HasProp Corner (Icon ms) where
    type Prop Corner (Icon ms) = Bool
    getProp _ = corner
    setProp _ c i = i { corner = c }

instance HasProp Disabled (Icon ms) where
    type Prop Disabled (Icon ms) = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Fitted (Icon ms) where
    type Prop Fitted (Icon ms) = Bool
    getProp _ = fitted
    setProp _ f i = i { fitted = f }

instance HasProp Flipped (Icon ms) where
    type Prop Flipped (Icon ms) = Txt
    getProp _ = flipped
    setProp _ f i = i { flipped = f }

instance HasProp Inverted (Icon ms) where
    type Prop Inverted (Icon ms) = Bool
    getProp _ = inverted
    setProp _ inv i = i { inverted = inv }

instance HasProp Link (Icon ms) where
    type Prop Link (Icon ms) = Bool
    getProp _ = link
    setProp _ l i = i { link = l }

instance HasProp Loading (Icon ms) where
    type Prop Loading (Icon ms) = Bool
    getProp _ = loading
    setProp _ l i = i { loading = l }

instance HasProp Rotated (Icon ms) where
    type Prop Rotated (Icon ms) = Txt
    getProp _ = rotated
    setProp _ r i = i { rotated = r }

instance HasProp Size (Icon ms) where
    type Prop Size (Icon ms) = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , children :: [View ms]
    , classes :: [Txt]
    , attributes :: [Feature ms]
    , size :: Txt
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = I }

pattern Group :: Group ms -> View ms
pattern Group ig = View ig

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( size
                : "icons"
                : classes
                )
        in as (mergeClasses $ ClassList cs : attributes) children

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
