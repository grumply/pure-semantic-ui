module Semantic.Elements.Container
  ( module Properties
  , module Tools
  , Container(..), pattern Container, pattern TextContainer
  ) where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasFluidProp(..), pattern Fluid
  , HasTextAlignProp(..), pattern TextAlign
  )

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

instance HasAsProp (Container ms) where
    type AsProp (Container ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f c = c { as = f }

instance HasAttributesProp (Container ms) where
    type Attribute (Container ms) = Feature ms
    getAttributes = attributes
    setAttributes cs c = c { attributes = cs }

instance HasChildrenProp (Container ms) where
    type Child (Container ms) = View ms
    getChildren = children
    setChildren cs c = c { children = cs }

instance HasClassesProp (Container ms) where
    getClasses = classes
    setClasses cs c = c { classes = cs }

instance HasFluidProp (Container ms) where
    getFluid = fluid
    setFluid f c = c { fluid = f }

instance HasTextAlignProp (Container ms) where
    getTextAlign = textAlign
    setTextAlign ta c = c { textAlign = ta }
