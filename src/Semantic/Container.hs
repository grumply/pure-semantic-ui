module Semantic.Container
  ( module Properties
  , module Tools
  , Container(..), pattern Container, pattern TextContainer
  ) where

import Pure hiding (text,(#))

import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Fluid, Fluid(..)
  , pattern TextAlign, TextAlign(..)
  , pattern IsText, IsText(..)
  )

import Data.Function as Tools ((&))

data Container = Container_
  { as :: Features -> [View] -> View
  , children :: [View]
  , features :: Features
  , fluid :: Bool
  , text :: Bool
  , textAlign :: Txt
  } deriving (Generic)

instance Default Container where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Container :: Container -> Container
pattern Container c = c

pattern TextContainer c <- (getTextContainer -> (True,c)) where
    TextContainer c = setTextContainer c

{-# INLINE getTextContainer #-}
getTextContainer c =
    case c of
        View Container_ {..} -> (text,c)
        _                    -> (False,c)

{-# INLINE setTextContainer #-}
setTextContainer c =
    case c of
        View Container_ {..} -> View Container_ { text = True, .. }
        _                    -> c

instance Pure Container where
    view Container_ {..} =
        let cs =
              [ "ui"
              , text # "text"
              , fluid # "fluid"
              , textAlign
              , "container"
              ]
        in as (features & Classes cs) children

instance HasProp As Container where
    type Prop As Container = Features -> [View] -> View
    getProp _ = as
    setProp _ f c = c { as = f }

instance HasFeatures Container where
    getFeatures = features
    setFeatures cs c = c { features = cs }

instance HasChildren Container where
    getChildren = children
    setChildren cs c = c { children = cs }

instance HasProp Fluid Container where
    type Prop Fluid Container = Bool
    getProp _ = fluid
    setProp _ f c = c { fluid = f }

instance HasProp IsText Container where
    type Prop IsText Container = Bool
    getProp _ = text
    setProp _ t c = c { text = t }

instance HasProp TextAlign Container where
    type Prop TextAlign Container = Txt
    getProp _ = textAlign
    setProp _ ta c = c { textAlign = ta }
