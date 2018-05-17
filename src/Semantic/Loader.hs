module Semantic.Loader
  ( module Properties
  , module Tools
  , Loader(..), pattern Loader
  ) where

import GHC.Generics as G
import Pure.View hiding (active,disabled,inline,verticalAlign)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Disabled, Disabled(..)
  , pattern Inline, Inline(..)
  , pattern Inverted, Inverted(..)
  , pattern IsIndeterminate, IsIndeterminate(..)
  , pattern Size, Size(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Loader = Loader_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , disabled :: Bool
    , indeterminate :: Bool
    , inline :: Maybe Txt
    , inverted :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default Loader where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Loader :: Loader -> Loader
pattern Loader l = l

instance Pure Loader ms where
    render Loader_ {..} =
        let
            cs =
                ( "ui"
                : size
                : active # "active"
                : disabled # "disabled"
                : indeterminate # "indeterminate"
                : inverted # "inverted"
                : children # "text"
                : may (<>> "inline") inline
                : "loader"
                )
        in
            as
                : attributes
                )
                children

instance HasProp Active Loader where
    type Prop Active Loader = Bool
    getProp _ = active
    setProp _ a l = l { active = a }

instance HasProp As Loader where
    type Prop As Loader = Features -> [View] -> View
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasFeatures Loader where
    getFeatures = features
    setFeatures cs l = l { features = cs }

instance HasChildren Loader where
    getChildren = children
    setChildren cs l = l { children = cs }


instance HasProp Disabled Loader where
    type Prop Disabled Loader = Bool
    getProp _ = disabled
    setProp _ d l = l { disabled = d }

instance HasProp Inline Loader where
    type Prop Inline Loader = Maybe Txt
    getProp _ = inline
    setProp _ i l = l { inline = i }

instance HasProp IsIndeterminate Loader where
    type Prop IsIndeterminate Loader = Bool
    getProp _ = indeterminate
    setProp _ i l = l { indeterminate = i }

instance HasProp Inverted Loader where
    type Prop Inverted Loader = Bool
    getProp _ = inverted
    setProp _ i l = l { inverted = i }

instance HasProp Size Loader where
    type Prop Size Loader = Txt
    getProp _ = size
    setProp _ s l = l { size = s }
