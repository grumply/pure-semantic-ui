module Semantic.Elements.Step.StepTitle where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data StepTitle ms = StepTitle_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StepTitle ms) where
    def = (G.to gdef) { as = Div }

pattern StepTitle :: Typeable ms => StepTitle ms -> View ms
pattern StepTitle st = View st

instance Typeable ms => Pure StepTitle ms where
    render StepTitle_ {..} =
        let
            cs =
                ( "title"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children