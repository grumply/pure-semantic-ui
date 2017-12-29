module Semantic.Collections.Table where

import GHC.Generics as G
import Pure.View hiding (Table)
import qualified Pure.View as HTML

import Semantic.Utils

data Table ms = Table_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , basic :: Maybe Txt
    , celled :: Bool
    , collapsing :: Bool
    , color :: Txt
    , columns :: Width
    , compact :: Maybe Txt
    , definition :: Bool
    , fixed :: Bool
    , inverted :: Bool
    , padded :: Maybe Txt
    , selectable :: Bool
    , singleLine :: Bool
    , size :: Txt
    , sortable :: Bool
    , stackable :: Bool
    , striped :: Bool
    , structured :: Bool
    , textAlign :: Txt
    , unstackable :: Bool
    , verticalAlign :: Txt 
    } deriving (Generic)

instance Default (Table ms) where
    def = (G.to gdef) { as = HTML.Table }

pattern Table :: Typeable ms => Table ms -> View ms
pattern Table t = View t

instance Typeable ms => Pure Table ms where
    render Table_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : celled # "celled"
                : collapsing # "collapsing"
                : definition # "definition"
                : fixed # "fixed"
                : inverted # "inverted"
                : selectable # "selectable"
                : singleLine # "single line"
                : sortable # "sortable"
                : stackable # "stackable"
                : striped # "striped"
                : structured # "structured"
                : unstackable # "unstackable"
                : may (<>> "attached") attached
                : may (<>> "basic") basic
                : may (<>> "compact") compact
                : may (<>> "padded") padded
                : textAlign
                : verticalAlign
                : widthProp columns "column" def
                : "table"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children