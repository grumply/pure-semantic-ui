module Semantic.Loader
  ( module Properties
  , module Tools
  , Loader(..), pattern Loader
  ) where

import GHC.Generics as G
import Pure.View hiding (active,disabled,inline,verticalAlign)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Disabled, Disabled(..)
  , pattern Inline, Inline(..)
  , pattern Inverted, Inverted(..)
  , pattern IsIndeterminate, IsIndeterminate(..)
  , pattern Size, Size(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Loader ms = Loader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , indeterminate :: Bool
    , inline :: Maybe Txt
    , inverted :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default (Loader ms) where
    def = (G.to gdef) { as = Div }

pattern Loader :: Loader ms -> View ms
pattern Loader l = View l

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
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp Active (Loader ms) where
    type Prop Active (Loader ms) = Bool
    getProp _ = active
    setProp _ a l = l { active = a }

instance HasProp As (Loader ms) where
    type Prop As (Loader ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasProp Attributes (Loader ms) where
    type Prop Attributes (Loader ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs l = l { attributes = cs }

instance HasProp Children (Loader ms) where
    type Prop Children (Loader ms) = [View ms]
    getProp _ = children
    setProp _ cs l = l { children = cs }

instance HasProp Classes (Loader ms) where
    type Prop Classes (Loader ms) = [Txt]
    getProp _ = classes
    setProp _ cs l = l { classes = cs }

instance HasProp Disabled (Loader ms) where
    type Prop Disabled (Loader ms) = Bool
    getProp _ = disabled
    setProp _ d l = l { disabled = d }

instance HasProp Inline (Loader ms) where
    type Prop Inline (Loader ms) = Maybe Txt
    getProp _ = inline
    setProp _ i l = l { inline = i }

instance HasProp IsIndeterminate (Loader ms) where
    type Prop IsIndeterminate (Loader ms) = Bool
    getProp _ = indeterminate
    setProp _ i l = l { indeterminate = i }

instance HasProp Inverted (Loader ms) where
    type Prop Inverted (Loader ms) = Bool
    getProp _ = inverted
    setProp _ i l = l { inverted = i }

instance HasProp Size (Loader ms) where
    type Prop Size (Loader ms) = Txt
    getProp _ = size
    setProp _ s l = l { size = s }
