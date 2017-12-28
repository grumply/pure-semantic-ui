module Semantic.Collections.Grid.GridColumn where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data GridColumn ms = GridColumn_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , computer :: Width
    , floated :: Txt
    , largeScreen :: Width
    , mobile :: Width
    , only :: [Txt]
    , stretched :: Bool
    , tablet :: Width
    , textAlign :: Txt
    , verticalAlign :: Txt
    , widescreen :: Width
    , width :: Width
    } deriving (Generic)

instance Default (GridColumn ms) where
    def = (G.to gdef) { as = Div }

pattern GridColumn :: Typeable ms => GridColumn ms -> View ms
pattern GridColumn gc = View gc

instance Typeable ms => Pure GridColumn ms where
    render GridColumn_ {..} =
        let
            cs =
                ( color
                : stretched # "stretched"
                : multiProp only "only"
                : textAlign
                : floated # (floated <>> "floated")
                : verticalAlign
                : widthProp computer "wide computer" def
                : widthProp largeScreen "wide large screen" def
                : widthProp mobile "wide mobile" def
                : widthProp widescreen "wide widescreen" def
                : widthProp width "wide" def
                : "column"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                ) 
                children
