module Semantic.Container
  ( module Properties
  , module Tools
  , Container(..), pattern Container, pattern TextContainer
  ) where

import GHC.Generics as G
import Pure.View hiding (textAlign,text)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Fluid, Fluid(..)
  , pattern TextAlign, TextAlign(..)
  , pattern IsText, IsText(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Container ms = Container_
  { as :: [Feature ms] -> [View ms] -> View ms
  , children :: [View ms]
  , attributes :: [Feature ms]
  , classes :: [Txt]
  , fluid :: Bool
  , text :: Bool
  , textAlign :: Txt
  } deriving (Generic)

instance Default (Container ms) where
    def = (G.to gdef) { as = Div }

pattern Container :: Container ms -> View ms
pattern Container c = View c

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

instance Pure Container ms where
    render Container_ {..} =
        let cs =
              ( "ui"
              : text # "text"
              : fluid # "fluid"
              : textAlign
              : "container"
              : classes
              )
        in as (ClassList cs : attributes) children

instance HasProp As (Container ms) where
    type Prop As (Container ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f c = c { as = f }

instance HasProp Attributes (Container ms) where
    type Prop Attributes (Container ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs c = c { attributes = cs }

instance HasProp Children (Container ms) where
    type Prop Children (Container ms) = [View ms]
    getProp _ = children
    setProp _ cs c = c { children = cs }

instance HasProp Classes (Container ms) where
    type Prop Classes (Container ms) = [Txt]
    getProp _ = classes
    setProp _ cs c = c { classes = cs }

instance HasProp Fluid (Container ms) where
    type Prop Fluid (Container ms) = Bool
    getProp _ = fluid
    setProp _ f c = c { fluid = f }

instance HasProp IsText (Container ms) where
    type Prop IsText (Container ms) = Bool
    getProp _ = text
    setProp _ t c = c { text = t }

instance HasProp TextAlign (Container ms) where
    type Prop TextAlign (Container ms) = Txt
    getProp _ = textAlign
    setProp _ ta c = c { textAlign = ta }
