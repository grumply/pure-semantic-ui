module Semantic.Rail
  ( module Properties
  , module Tools
  , Rail(..), pattern Rail
  ) where

import GHC.Generics as G
import Pure.View hiding (position,verticalAlign)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Close, Close(..)
  , pattern Dividing, Dividing(..)
  , pattern Internal, Internal(..)
  , pattern Position, Position(..)
  , pattern Size, Size(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Rail ms = Rail_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Bool
    , close :: Maybe Txt
    , dividing :: Bool
    , internal :: Bool
    , position :: Txt
    , size :: Txt
    } deriving (Generic)

instance Default (Rail ms) where
    def = (G.to gdef) { as = Div }

pattern Rail :: Rail ms -> View ms
pattern Rail r = View r

instance Pure Rail ms where
    render Rail_ {..} =
        let
            cs =
                ( "ui"
                : position
                : size
                : attached # "attached"
                : dividing # "dividing"
                : internal # "internal"
                : may (<>> "close") close
                : "rail"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Rail ms) where
    type Prop As (Rail ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f r = r { as = f }

instance HasProp Attached (Rail ms) where
    type Prop Attached (Rail ms) = Bool
    getProp _ = attached
    setProp _ attach r = r { attached = attach }

instance HasProp Attributes (Rail ms) where
    type Prop Attributes (Rail ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs r = r { attributes = cs }

instance HasProp Children (Rail ms) where
    type Prop Children (Rail ms) = [View ms]
    getProp _ = children
    setProp _ cs r = r { children = cs }

instance HasProp Classes (Rail ms) where
    type Prop Classes (Rail ms) = [Txt]
    getProp _ = classes
    setProp _ cs r = r { classes = cs }

instance HasProp Close (Rail ms) where
    type Prop Close (Rail ms) = Maybe Txt
    getProp _ = close
    setProp _ c r = r { close = c }

instance HasProp Dividing (Rail ms) where
    type Prop Dividing (Rail ms) = Bool
    getProp _ = dividing
    setProp _ d r = r { dividing = d }

instance HasProp Internal (Rail ms) where
    type Prop Internal (Rail ms) = Bool
    getProp _ = internal
    setProp _ i r = r { internal = i }

instance HasProp Position (Rail ms) where
    type Prop Position (Rail ms) = Txt
    getProp _ = position
    setProp _ p r = r { position = p }

instance HasProp Size (Rail ms) where
    type Prop Size (Rail ms) = Txt
    getProp _ = size
    setProp _ s r = r { size = s }
