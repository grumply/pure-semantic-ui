module Semantic.Views.Card where

import GHC.Generics as G
import Pure.View hiding (color,onClick)

import Semantic.Utils

data Card ms = Card_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , color :: Txt
    , fluid :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    , raised :: Bool
    } deriving (Generic)

instance Default (Card ms) where
    def = (G.to gdef) { as = Div }

pattern Card :: Typeable ms => Card ms -> View ms
pattern Card a = View a

instance Typeable ms => Pure Card ms where
    render Card_ {..} =
        let
            e = onClick ? A $ as
            cs =
                ( "ui"
                : color
                : centered # "centered"
                : fluid # "fluid"
                : link # "link"
                : raised # "raised"
                : "card"
                : classes
                )
        in
            e
                ( ClassList cs
                : ref
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children


