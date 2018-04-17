module Semantic.Divider
  ( module Properties
  , module Tools
  , Divider(..), pattern Divider
  ) where

import GHC.Generics as G
import Pure.View as View hiding (hidden,horizontal,vertical)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Clearing, Clearing(..)
  , pattern Fitted, Fitted(..)
  , pattern Hidden, Hidden(..)
  , pattern Horizontal, Horizontal(..)
  , pattern Inverted, Inverted(..)
  , pattern Section, Section(..)
  , pattern Vertical, Vertical(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Divider ms = Divider_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , clearing :: Bool
    , fitted :: Bool
    , hidden :: Bool
    , horizontal :: Bool
    , inverted :: Bool
    , section :: Bool
    , vertical :: Bool
    } deriving (Generic)

instance Default (Divider ms) where
    def = (G.to gdef) { as = Div }

pattern Divider :: Divider ms -> View ms
pattern Divider d = View d

instance Pure Divider ms where
    render Divider_ {..} =
        let
            cs =
                ( "ui"
                : clearing # "clearing"
                : fitted # "fitted"
                : hidden # "hidden"
                : horizontal # "horizontal"
                : inverted # "inverted"
                : section # "section"
                : vertical # "verical"
                : "divider"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Divider ms) where
    type Prop As (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f d = d { as = f }

instance HasProp Attributes (Divider ms) where
    type Prop Attributes (Divider ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs d = d { attributes = cs }

instance HasProp Children (Divider ms) where
    type Prop Children (Divider ms) = [View ms]
    getProp _ = children
    setProp _ cs d = d { children = cs }

instance HasProp Classes (Divider ms) where
    type Prop Classes (Divider ms) = [Txt]
    getProp _ = classes
    setProp _ cs d = d { classes = cs }

instance HasProp Clearing (Divider ms) where
    type Prop Clearing (Divider ms) = Bool
    getProp _ = clearing
    setProp _ c d = d { clearing = c }

instance HasProp Fitted (Divider ms) where
    type Prop Fitted (Divider ms) = Bool
    getProp _ = fitted
    setProp _ f d = d { fitted = f }

instance HasProp Hidden (Divider ms) where
    type Prop Hidden (Divider ms) = Bool
    getProp _ = hidden
    setProp _ h d = d { hidden = h }

instance HasProp Horizontal (Divider ms) where
    type Prop Horizontal (Divider ms) = Bool
    getProp _ = horizontal
    setProp _ h d = d { horizontal = h }

instance HasProp Inverted (Divider ms) where
    type Prop Inverted (Divider ms) = Bool
    getProp _ = inverted
    setProp _ i d = d { inverted = i }

instance HasProp Section (Divider ms) where
    type Prop Section (Divider ms) = Bool
    getProp _ = section
    setProp _ s d = d { section = s }

instance HasProp Vertical (Divider ms) where
    type Prop Vertical (Divider ms) = Bool
    getProp _ = vertical
    setProp _ v d = d { vertical = v }
