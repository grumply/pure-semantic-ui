module Semantic.Collections.Menu.MenuMenu where

import GHC.Generics as G
import Pure.View hiding (position)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Position

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

instance HasAsProp (MenuMenu ms) where
    type AsProp (MenuMenu ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mm = mm { as = a }

instance HasAttributesProp (MenuMenu ms) where
    type Attribute (MenuMenu ms) = Feature ms
    getAttributes = attributes
    setAttributes as mm = mm { attributes = as }

instance HasChildrenProp (MenuMenu ms) where
    type Child (MenuMenu ms) = View ms
    getChildren = children
    setChildren cs mm = mm { children = cs }

instance HasClassesProp (MenuMenu ms) where
    getClasses = classes
    setClasses cs mm = mm { classes = cs }

instance HasPositionProp (MenuMenu ms) where
    getPosition = position
    setPosition p mm = mm { position = p }