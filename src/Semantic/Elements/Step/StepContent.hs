module Semantic.Elements.Step.StepContent where

import GHC.Generics as G
import Pure.View

import Semantic.Properties.Utils

data StepContent ms = StepContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (StepContent ms) where
    def = (G.to gdef) { as = Div }

pattern StepContent :: Typeable ms => StepContent ms -> View ms
pattern StepContent sc = View sc

instance Typeable ms => Pure StepContent ms where
    render StepContent_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
