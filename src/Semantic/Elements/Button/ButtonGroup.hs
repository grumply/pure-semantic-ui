module Semantic.Elements.Button.ButtonGroup where

import GHC.Generics as G
import Pure.View hiding (Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon

import Semantic.Extensions.As
import Semantic.Extensions.Attached
import Semantic.Extensions.Attributes
import Semantic.Extensions.Basic
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data ButtonGroup ms = ButtonGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attached :: Maybe Txt
    , attributes :: [Feature ms]
    , basic :: Bool
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , compact :: Bool
    , floated :: Txt
    , fluid :: Bool
    , inverted :: Bool
    , labeled :: Bool
    , negative :: Bool
    , positive :: Bool
    , primary :: Bool
    , secondary :: Bool
    , size :: Txt
    , toggle :: Bool
    , vertical :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (ButtonGroup ms) where
    def = (G.to gdef) { as = Div }

pattern ButtonGroup :: Typeable ms => ButtonGroup ms -> View ms
pattern ButtonGroup bc = View bc

instance Typeable ms => Pure ButtonGroup ms where
    render ButtonGroup_ {..} =
        let
            icon =
                foldPures (\(Icon_ {}) -> const True) False children
            
            cs =
                ( "ui"
                : color
                : size
                : basic # "basic"
                : compact # "compact"
                : fluid # "fluid"
                : icon # "icon"
                : inverted # "inverted"
                : labeled # "labeled"
                : negative # "negative"
                : positive # "positive"
                : primary # "primary"
                : secondary # "secondary"
                : toggle # "toggle"
                : vertical # "vertical"
                : useKeyOrValueAndKey attached "attached"
                : floated # ("floated" <<>> floated)
                : widthProp widths def def
                : "buttons"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAs (ButtonGroup ms) where
    type Constructor (ButtonGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f bg = bg { as = f }

instance HasAttached (ButtonGroup ms) where
    type Attach (ButtonGroup ms) = Maybe Txt
    getAttached = attached
    setAttached attach bg = bg { attached = attach }

instance HasAttributes (ButtonGroup ms) where
    type Attribute (ButtonGroup ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs bg = bg { attributes = cs }

instance HasBasic (ButtonGroup ms) where
    getBasic = basic
    setBasic b bg = bg { basic = b }

instance HasChildren (ButtonGroup ms) where
    type Child (ButtonGroup ms) = View ms
    getChildren = children
    setChildren cs bg = bg { children = cs }

instance HasClasses (ButtonGroup ms) where
    getClasses = classes
    setClasses cs bg = bg { classes = cs }