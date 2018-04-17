module Semantic.Flag
  ( module Properties
  , module Tools
  , Flag(..), pattern Flag
  ) where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Classes, Classes(..)
  , pattern Name, Name(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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

instance HasProp As (Flag ms) where
    type Prop As (Flag ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a f = f { as = a }

instance HasProp Attributes (Flag ms) where
    type Prop Attributes (Flag ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as f = f { attributes = as }

instance HasProp Classes (Flag ms) where
    type Prop Classes (Flag ms) = [Txt]
    getProp _ = classes
    setProp _ cs f = f { classes = cs }

instance HasProp Name (Flag ms) where
    type Prop Name (Flag ms) = Txt
    getProp _ = name
    setProp _ n f = f { name = n }
