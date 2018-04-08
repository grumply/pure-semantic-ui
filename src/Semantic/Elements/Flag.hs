module Semantic.Elements.Flag where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasClassesProp(..), pattern Classes
  , HasNameProp(..), pattern Name
  )

data Flag ms = Flag_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , name :: Txt
    } deriving (Generic)

instance Default (Flag ms) where
    def = (G.to gdef) { as = I }

pattern Flag :: Flag ms -> View ms
pattern Flag f = View f

instance Pure Flag ms where
    render Flag_ {..} =
        let
            cs =
                ( name
                : "flag"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                []

instance HasAsProp (Flag ms) where
    type AsProp (Flag ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a f = f { as = a }

instance HasAttributesProp (Flag ms) where
    type Attribute (Flag ms) = Feature ms
    getAttributes = attributes
    setAttributes as f = f { attributes = as }

instance HasClassesProp (Flag ms) where
    getClasses = classes
    setClasses cs f = f { classes = cs }

instance HasNameProp (Flag ms) where
    getName = name
    setName n f = f { name = n }
