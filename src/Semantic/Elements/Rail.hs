module Semantic.Elements.Rail where

import GHC.Generics as G
import Pure.View hiding (verticalAlign)

import Semantic.Utils

data Rail ms = Rail_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Bool
    , close :: Maybe Txt
    , dividing :: Bool
    , internal :: Bool
    , position :: Txt
    , size :: Txt
    } deriving (Generic)

instance Default (Rail ms) where
    def = (G.to gdef) { as = Div }

pattern Rail :: Typeable ms => Rail ms -> View ms
pattern Rail r = View r

instance Typeable ms => Pure Rail ms where
    render Rail_ {..} =
        let
            cs =
                ( "ui"
                : position
                : size
                : attached # "attached"
                : dividing # "dividing"
                : internal # "internal"
                : may (<>> "close") close
                : "rail"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children