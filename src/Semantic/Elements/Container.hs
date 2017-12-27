module Semantic.Elements.Container where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

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

pattern Container :: Typeable ms => Container ms -> View ms
pattern Container c = View c

instance Typeable ms => Pure Container ms where
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

instance HasAs (Container ms) where
    type Constructor (Container ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f c = c { as = f }

instance HasAttributes (Container ms) where
    type Attribute (Container ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs c = c { attributes = cs }

instance HasChildren (Container ms) where
    type Child (Container ms) = View ms
    getChildren = children
    setChildren cs c = c { children = cs }

instance HasClasses (Container ms) where
    getClasses = classes
    setClasses cs c = c { classes = cs }