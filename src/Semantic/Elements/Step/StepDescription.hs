module Semantic.Elements.Step.StepDescription where

import GHC.Generics as G
import Pure.View

import Semantic.Properties.Utils

data StepDescription ms = StepDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StepDescription ms) where
    def = (G.to gdef) { as = Div }

pattern StepDescription :: Typeable ms => StepDescription ms -> View ms
pattern StepDescription sd = View sd

instance Typeable ms => Pure StepDescription ms where
    render StepDescription_ {..} =
        let
            cs =
                ( "description"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children