module Semantic.Views.Advertisement
  ( module Properties
  , module Tools
  , Advertisement(..), pattern Advertisement
  ) where

import GHC.Generics as G
import Pure.View hiding (unit)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Centered, Centered(..)
  , pattern Test, Test(..)
  , pattern Unit, Unit(..)
  )

import Data.Function as Tool ((&))
import Pure.Data.Default as Tools

data Advertisement ms = Advertisement_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , test :: Txt
    , unit :: Txt
    } deriving (Generic)

instance Default (Advertisement ms) where
    def = (G.to gdef) { as = Div }

pattern Advertisement :: Advertisement ms -> View ms
pattern Advertisement a = View a

instance Pure Advertisement ms where
    render Advertisement_ {..} =
        let
            cs =
                ( "ui"
                : unit
                : centered # "centered"
                : test # "test"
                : "ad"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : test # (Property "data-text" test)
                : attributes
                )
                children

instance HasProp As (Advertisement ms) where
    type Prop As (Advertisement ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ad = ad { as = a }

instance HasProp Attributes (Advertisement ms) where
    type Prop Attributes (Advertisement ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as a = a { attributes = as }

instance HasProp Children (Advertisement ms) where
    type Prop Children (Advertisement ms) = [View ms]
    getProp _ = children
    setProp _ cs a = a { children = cs }

instance HasProp Classes (Advertisement ms) where
    type Prop Classes (Advertisement ms) = [Txt]
    getProp _ = classes
    setProp _ cs a = a { classes = cs }

instance HasProp Centered (Advertisement ms) where
    type Prop Centered (Advertisement ms) = Bool
    getProp _ = centered
    setProp _ c a = a { centered = c }

instance HasProp Test (Advertisement ms) where
    type Prop Test (Advertisement ms) = Txt
    getProp _ = test
    setProp _ t a = a { test = t }

instance HasProp Unit (Advertisement ms) where
    type Prop Unit (Advertisement ms) = Txt
    getProp _ = unit
    setProp _ u a = a { unit = u }
