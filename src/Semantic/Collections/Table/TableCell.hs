module Semantic.Collections.Table.TableCell where

import GHC.Generics as G
import Pure.View hiding (active,disabled,textAlign,verticalAlign,width)

data TableCell ms = TableCell_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , collapsing :: Bool
    , disabled :: Bool
    , error :: Bool
    , negative :: Bool
    , positive :: Bool
    , selectable :: Bool
    , singleLine :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    , warning :: Bool
    , width :: Width
    } deriving (Generic)

instance Default (TableCell ms) where
    def = (G.to gdef) { as = Td }

pattern TableCell :: Typeable ms => TableCell ms -> View ms
pattern TableCell tc = View tc

instance Typeable ms => Pure TableCell ms where
    render TableCell_ {..} =
        let
            cs =
                ( active # "active"
                : collapsing # "collapsing"
                : disabled # "disabled"
                : error # "error"
                : negative # "negative"
                : positive # "positive"
                : selectable # "selectable"
                : singleLine # "single line"
                : warning # "warning"
                : textAlign
                : verticalAlign
                : widthProp width "wide" def
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
