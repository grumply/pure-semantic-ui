module Semantic.Tab
  ( module Properties
  , module Tools
  , Tab(..), pattern Tab
  , Pane(..), pattern Pane
  ) where

import Pure hiding (Tab,(#),active)

import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Active, Active(..)
  , pattern Loading, Loading(..)
  )

import Semantic.Properties
  ( pattern Attached, Attached(..) )

import Semantic.Segment (pattern Segment)

import Data.Function as Tools ((&))

data Tab = Tab_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Tab where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Tab :: Tab -> Tab
pattern Tab t = t

instance Pure Tab where
  view Tab_ {..} = as features children

instance HasProp As Tab where
    type Prop As Tab = Features -> [View] -> View
    getProp _ = as
    setProp _ f t = t { as = f }

instance HasFeatures Tab where
    getFeatures = features
    setFeatures cs t = t { features = cs }

instance HasChildren Tab where
    getChildren = children
    setChildren cs t = t { children = cs }

data Pane = Pane_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , loading :: Bool
    } deriving (Generic)

instance Default Pane where
    def = (G.to gdef)
        { as = \fs cs -> Segment def <| Attached "bottom" . setFeatures fs |> cs
        }

pattern Pane :: Pane -> Pane
pattern Pane tp = tp

instance Pure Pane where
    view Pane_ {..} =
        let
            cs =
                [ active # "active"
                , loading # "loading"
                , "tab"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Pane where
    type Prop As Pane = Features -> [View] -> View
    getProp _ = as
    setProp _ f tp = tp { as = f }

instance HasFeatures Pane where
    getFeatures = features
    setFeatures cs tp = tp { features = cs }

instance HasChildren Pane where
    getChildren = children
    setChildren cs tp = tp { children = cs }

instance HasProp Active Pane where
    type Prop Active Pane = Bool
    getProp _ = active
    setProp _ a tp = tp { active = a }

instance HasProp Loading Pane where
    type Prop Loading Pane = Bool
    getProp _ = loading
    setProp _ l tp = tp { loading = l }
