module Semantic.Collections.Grid where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

data Grid ms = Grid_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , celled :: Maybe Txt
    , centered :: Bool
    , columns :: Width
    , container :: Bool
    , divided :: Maybe Txt
    , doubling :: Bool
    , inverted :: Bool
    , padded :: Maybe Txt
    , relaxed :: Maybe Txt
    , reversed :: [Txt]
    , stackable :: Bool
    , stretched :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (Grid ms) where
    def = (G.to gdef) { as = Div }

pattern Grid :: Typeable ms => Grid ms -> View ms
pattern Grid g = View g

instance Typeable ms => Pure Grid ms where
    render Grid_ {..} =
        let
            cs =
                ( "ui"
                : centered # "centered"
                : container # "container"
                : doubling # "doubling"
                : inverted # "inverted"
                : stackable # "stackable"
                : stretched # "stretched"
                : may (<>> "celled") celled
                : may (<>> "divided") divided
                : may (<>> "padded") padded
                : may (<>> "relaxed") relaxed
                : multiProp reversed "reversed"
                : textAlign
                : verticalAlign
                : widthProp columns "column" True
                : "grid"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
