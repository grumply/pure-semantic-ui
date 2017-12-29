module Semantic.Collections.Menu.MenuMenu where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data MenuMenu ms = MenuMenu_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , position :: Txt
    } deriving (Generic)

instance Default (MenuMenu ms) where
    def = (G.to gdef) { as = Div }

pattern MenuMenu :: Typeable ms => MenuMenu ms -> View ms
pattern MenuMenu mm = View mm

instance Typeable ms => Pure MenuMenu ms where
    render MenuMenu_ {..} =
        let
            cs =
                ( position
                : "menu"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
