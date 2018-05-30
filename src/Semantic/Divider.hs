module Semantic.Divider
  ( module Properties
  , module Tools
  , Divider(..), pattern Divider
  ) where

import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
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

data Divider = Divider_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , clearing :: Bool
    , fitted :: Bool
    , hidden :: Bool
    , horizontal :: Bool
    , inverted :: Bool
    , section :: Bool
    , vertical :: Bool
    } deriving (Generic)

instance Default Divider where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Divider :: Divider -> Divider
pattern Divider d = d

instance Pure Divider where
    view Divider_ {..} =
        let
            cs =
                [ "ui"
                , clearing # "clearing"
                , fitted # "fitted"
                , hidden # "hidden"
                , horizontal # "horizontal"
                , inverted # "inverted"
                , section # "section"
                , vertical # "verical"
                , "divider"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Divider where
    type Prop As Divider = Features -> [View] -> View
    getProp _ = as
    setProp _ f d = d { as = f }

instance HasFeatures Divider where
    getFeatures = features
    setFeatures cs d = d { features = cs }

instance HasChildren Divider where
    getChildren = children
    setChildren cs d = d { children = cs }

instance HasProp Clearing Divider where
    type Prop Clearing Divider = Bool
    getProp _ = clearing
    setProp _ c d = d { clearing = c }

instance HasProp Fitted Divider where
    type Prop Fitted Divider = Bool
    getProp _ = fitted
    setProp _ f d = d { fitted = f }

instance HasProp Hidden Divider where
    type Prop Hidden Divider = Bool
    getProp _ = hidden
    setProp _ h d = d { hidden = h }

instance HasProp Horizontal Divider where
    type Prop Horizontal Divider = Bool
    getProp _ = horizontal
    setProp _ h d = d { horizontal = h }

instance HasProp Inverted Divider where
    type Prop Inverted Divider = Bool
    getProp _ = inverted
    setProp _ i d = d { inverted = i }

instance HasProp Section Divider where
    type Prop Section Divider = Bool
    getProp _ = section
    setProp _ s d = d { section = s }

instance HasProp Vertical Divider where
    type Prop Vertical Divider = Bool
    getProp _ = vertical
    setProp _ v d = d { vertical = v }
