module Semantic.Flag
  ( module Properties
  , module Tools
  , Flag(..), pattern Flag
  ) where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Name, Name(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Flag = Flag_
    { as :: Features -> [View] -> View
    , features :: Features
    , name :: Txt
    } deriving (Generic)

instance Default Flag where
    def = (G.to gdef) { as = \fs cs -> I & Features fs & Children cs }

pattern Flag :: Flag -> Flag
pattern Flag f = f

instance Pure Flag where
    render Flag_ {..} =
        let
            cs =
                ( name
                : "flag"
                )
        in
            as
                : attributes
                )
                []

instance HasProp As Flag where
    type Prop As Flag = Features -> [View] -> View
    getProp _ = as
    setProp _ a f = f { as = a }

instance HasFeatures Flag where
    getFeatures = features
    setFeatures as f = f { features = as }


instance HasProp Name Flag where
    type Prop Name Flag = Txt
    getProp _ = name
    setProp _ n f = f { name = n }
