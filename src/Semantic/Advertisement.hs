module Semantic.Advertisement
  ( module Properties
  , module Tools
  , Advertisement(..), pattern Advertisement
  ) where

import Pure

import GHC.Generics as G (Generic,to)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Centered, Centered(..)
  , pattern Test, Test(..)
  , pattern Unit, Unit(..)
  )

import Data.Function as Tools ((&))

data Advertisement = Advertisement_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , centered :: Bool
    , test :: Txt
    , unit :: Txt
    } deriving (Generic)

instance Default Advertisement where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Advertisement :: Advertisement -> Advertisement
pattern Advertisement a = a

instance Pure Advertisement where
    view Advertisement_ {..} =
        let
            property
              | test /= def = Property "data-text" test
              | otherwise   = id

            cs =
                [ "ui"
                , unit
                , centered # "centered"
                , (test /= def) # "test"
                , "ad"
                ]
        in
            as (features & Classes cs & property) children

instance HasProp As Advertisement where
    type Prop As Advertisement = Features -> [View] -> View
    getProp _ = as
    setProp _ a ad = ad { as = a }

instance HasFeatures Advertisement where
    getFeatures = features
    setFeatures fs a = a { features = fs }

instance HasChildren Advertisement where
    getChildren = children
    setChildren cs a = a { children = cs }

instance HasProp Centered Advertisement where
    type Prop Centered Advertisement = Bool
    getProp _ = centered
    setProp _ c a = a { centered = c }

instance HasProp Test Advertisement where
    type Prop Test Advertisement = Txt
    getProp _ = test
    setProp _ t a = a { test = t }

instance HasProp Unit Advertisement where
    type Prop Unit Advertisement = Txt
    getProp _ = unit
    setProp _ u a = a { unit = u }
