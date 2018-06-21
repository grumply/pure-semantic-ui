module Semantic.Rail
  ( module Properties
  , module Tools
  , Rail(..), pattern Rail
  ) where

import Pure hiding (position)

import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Close, Close(..)
  , pattern Dividing, Dividing(..)
  , pattern Internal, Internal(..)
  , pattern Position, Position(..)
  , pattern Size, Size(..)
  )

import Data.Function as Tools ((&))

data Rail = Rail_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , attached :: Bool
    , close :: Maybe Txt
    , dividing :: Bool
    , internal :: Bool
    , position :: Txt
    , size :: Txt
    } deriving (Generic)

instance Default Rail where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Rail :: Rail -> Rail
pattern Rail r = r

instance Pure Rail where
    view Rail_ {..} =
        let
            cs =
                [ "ui"
                , position
                , size
                , attached # "attached"
                , dividing # "dividing"
                , internal # "internal"
                , maybe "" (<>> "close") close
                , "rail"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Rail where
    type Prop As Rail = Features -> [View] -> View
    getProp _ = as
    setProp _ f r = r { as = f }

instance HasProp Attached Rail where
    type Prop Attached Rail = Bool
    getProp _ = attached
    setProp _ attach r = r { attached = attach }

instance HasFeatures Rail where
    getFeatures = features
    setFeatures cs r = r { features = cs }

instance HasChildren Rail where
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasProp Close Rail where
    type Prop Close Rail = Maybe Txt
    getProp _ = Semantic.Rail.close
    setProp _ c r = r { Semantic.Rail.close = c }

instance HasProp Dividing Rail where
    type Prop Dividing Rail = Bool
    getProp _ = dividing
    setProp _ d r = r { dividing = d }

instance HasProp Internal Rail where
    type Prop Internal Rail = Bool
    getProp _ = internal
    setProp _ i r = r { internal = i }

instance HasProp Position Rail where
    type Prop Position Rail = Txt
    getProp _ = position
    setProp _ p r = r { position = p }

instance HasProp Size Rail where
    type Prop Size Rail = Txt
    getProp _ = size
    setProp _ s r = r { size = s }
