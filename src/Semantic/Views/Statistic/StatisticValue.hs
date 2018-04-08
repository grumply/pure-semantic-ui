module Semantic.Views.Statistic.StatisticValue where

import GHC.Generics as G
import Pure.View hiding (text)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasIsTextProp(..), pattern IsText
  )

data StatisticValue ms = StatisticValue_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (StatisticValue ms) where
    def = (G.to gdef) { as = Div }

pattern StatisticValue :: StatisticValue ms -> View ms
pattern StatisticValue sv = View sv

instance Pure StatisticValue ms where
    render StatisticValue_ {..} =
        let
            cs =
                ( text # "text"
                : "value"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (StatisticValue ms) where
    type AsProp (StatisticValue ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sv = sv { as = a }

instance HasAttributesProp (StatisticValue ms) where
    type Attribute (StatisticValue ms) = Feature ms
    getAttributes = attributes
    setAttributes as sv = sv { attributes = as }

instance HasChildrenProp (StatisticValue ms) where
    type Child (StatisticValue ms) = View ms
    getChildren = children
    setChildren cs sv = sv { children = cs }

instance HasClassesProp (StatisticValue ms) where
    getClasses = classes
    setClasses cs sv = sv { classes = cs }

instance HasIsTextProp (StatisticValue ms) where
    getIsText = text
    setIsText it sv = sv { text = it }
