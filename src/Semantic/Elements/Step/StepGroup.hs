module Semantic.Elements.Step.StepGroup where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data StepGroup ms = StepGroup_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , fluid :: Bool
    , ordered :: Bool
    , size :: Txt
    , stackable :: Txt
    , unstackable :: Bool
    , vertical :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (StepGroup ms) where
    def = (G.to gdef) { as = Div }

pattern StepGroup :: Typeable ms => StepGroup ms -> View ms
pattern StepGroup sg = View sg

instance Typeable ms => Pure StepGroup ms where
    render StepGroup_ {..} =
        let
            cs =
                ( "ui"
                : size
                : fluid # "fluid"
                : ordered # "ordered"
                : unstackable # "unstackable"
                : vertical # "vertical"
                : may (<>> "attached") attached
                : stackable # stackable <>> "stackable"
                : widthProp widths def def
                : "steps"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children