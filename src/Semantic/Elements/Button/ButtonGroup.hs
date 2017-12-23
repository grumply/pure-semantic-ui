module Semantic.Elements.Button.ButtonGroup where

import GHC.Generics as G
import Pure.View hiding (Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon

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