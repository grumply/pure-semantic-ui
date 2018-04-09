module Semantic.Modules.Tab
  ( module Properties
  , module Tools
  , Tab(..), pattern Tab
  , Pane(..), pattern Pane
  ) where

import GHC.Generics as G
import Pure.View hiding (active,Tab)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Active, Active(..)
  , pattern Loading, Loading(..)
  )

import Semantic.Properties
  ( pattern Attached, Attached(..) )

import Semantic.Elements.Segment (pattern Segment)

data Tab ms = Tab_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Tab ms) where
    def = (G.to gdef) { as = Div }

pattern Tab :: Tab ms -> View ms
pattern Tab t = View t

instance Pure Tab ms where
    render Tab_ {..} = as ( ClassList classes : attributes ) children

instance HasProp As (Tab ms) where
    type Prop As (Tab ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f t = t { as = f }

instance HasProp Attributes (Tab ms) where
    type Prop Attributes (Tab ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs t = t { attributes = cs }

instance HasProp Children (Tab ms) where
    type Prop Children (Tab ms) = [View ms]
    getProp _ = children
    setProp _ cs t = t { children = cs }

instance HasProp Classes (Tab ms) where
    type Prop Classes (Tab ms) = [Txt]
    getProp _ = classes
    setProp _ cs t = t { classes = cs }

data Pane ms = Pane_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , loading :: Bool
    } deriving (Generic)

instance Default (Pane ms) where
    def = (G.to gdef)
        { as = \fs cs ->
            Segment $ def
                & Attached "bottom"
                & Attributes fs
                & Children cs
        }

pattern Pane :: Pane ms -> View ms
pattern Pane tp = View tp

instance Pure Pane ms where
    render Pane_ {..} =
        let
            cs =
                ( active # "active"
                : loading # "loading"
                : "tab"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Pane ms) where
    type Prop As (Pane ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f tp = tp { as = f }

instance HasProp Attributes (Pane ms) where
    type Prop Attributes (Pane ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs tp = tp { attributes = cs }

instance HasProp Children (Pane ms) where
    type Prop Children (Pane ms) = [View ms]
    getProp _ = children
    setProp _ cs tp = tp { children = cs }

instance HasProp Classes (Pane ms) where
    type Prop Classes (Pane ms) = [Txt]
    getProp _ = classes
    setProp _ cs tp = tp { classes = cs }

instance HasProp Active (Pane ms) where
    type Prop Active (Pane ms) = Bool
    getProp _ = active
    setProp _ a tp = tp { active = a }

instance HasProp Loading (Pane ms) where
    type Prop Loading (Pane ms) = Bool
    getProp _ = loading
    setProp _ l tp = tp { loading = l }
