module Semantic.Collections.Table.TableRow where

import GHC.Generics as G
import Pure.View hiding (active,disabled,textAlign,verticalAlign)

import Semantic.Utils
import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Disabled
import Semantic.Properties.Error
import Semantic.Properties.Negative
import Semantic.Properties.Positive
import Semantic.Properties.TextAlign
import Semantic.Properties.VerticalAlign
import Semantic.Properties.Warning

import Prelude hiding (error)

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
    def = (G.to gdef) { as = Tr }

pattern TableRow :: TableRow ms -> View ms
pattern TableRow tr = View tr

instance Pure TableRow ms where
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
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children
        
instance HasAsProp (TableRow ms) where
    type AsProp (TableRow ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tr = tr { as = a }

instance HasAttributesProp (TableRow ms) where
    type Attribute (TableRow ms) = Feature ms
    getAttributes = attributes
    setAttributes as tr = tr { attributes = as }

instance HasChildrenProp (TableRow ms) where
    type Child (TableRow ms) = View ms
    getChildren = children
    setChildren cs tr = tr { children = cs }

instance HasClassesProp (TableRow ms) where
    getClasses = classes
    setClasses cs tr = tr { classes = cs }

instance HasActiveProp (TableRow ms) where
    getActive = active
    setActive a tr = tr { active = a }

instance HasDisabledProp (TableRow ms) where
    getDisabled = disabled
    setDisabled d tr = tr { disabled = d }

instance HasErrorProp (TableRow ms) where
    getError = error
    setError e tr = tr { error = e }

instance HasNegativeProp (TableRow ms) where
    getNegative = negative
    setNegative n tr = tr { negative = n }

instance HasPositiveProp (TableRow ms) where
    getPositive = positive
    setPositive p tr = tr { positive = p }

instance HasTextAlignProp (TableRow ms) where
    getTextAlign = textAlign
    setTextAlign ta tr = tr { textAlign = ta }

instance HasVerticalAlignProp (TableRow ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va tr = tr { verticalAlign = va }

instance HasWarningProp (TableRow ms) where
    getWarning = warning
    setWarning w tr = tr { warning = w }
