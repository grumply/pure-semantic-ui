module Semantic.Collections.Table.TableRow where

import GHC.Generics as G
import Pure.View hiding (active,disabled,textAlign,verticalAlign)

data TableRow ms = TableRow_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , error :: Bool
    , negative :: Bool
    , positive :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    , warning :: Bool
    } deriving (Generic)

instance Default (TableRow ms) where
    def = (G.to gdef) { as = Td }

pattern TableRow :: Typeable ms => TableRow ms -> View ms
pattern TableRow tr = View tr

instance Typeable ms => Pure TableRow ms where
    render TableRow_ {..} =
        let
            cs =
                ( active # "active"
                : disabled # "disabled"
                : error # "error"
                : negative # "negative"
                : positive # "positive"
                : warning # "warning"
                : textAlign
                : verticalAlign
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
