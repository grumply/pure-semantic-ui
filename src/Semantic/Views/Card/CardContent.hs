module Semantic.Views.Card.CardContent where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

data CardContent ms = CardContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , extra :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardContent ms) where
    def = (G.to gdef) { as = Div }

pattern CardContent :: Typeable ms => CardContent ms -> View ms
pattern CardContent cc = View cc

instance Typeable ms => Pure CardContent ms where
    render CardContent_ {..} =
        let
            cs =
                ( extra # "extra"
                : textAlign
                : "content"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

