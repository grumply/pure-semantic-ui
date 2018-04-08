module Semantic.Collections.Table.TableCell where

import GHC.Generics as G
import Pure.View hiding (active,disabled,textAlign,verticalAlign,width)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActiveProp(..), pattern Active
  , HasCollapsingProp(..), pattern Collapsing
  , HasDisabledProp(..), pattern Disabled
  , HasErrorProp(..), pattern Error
  , HasNegativeProp(..), pattern Negative
  , HasPositiveProp(..), pattern Positive
  , HasSelectableProp(..), pattern Selectable
  , HasSingleLineProp(..), pattern SingleLine
  , HasTextAlignProp(..), pattern TextAlign
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasWarningProp(..), pattern Warning
  , HasWidthProp(..), pattern Width
  )

import Prelude hiding (error)

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

pattern TableCell :: TableCell ms -> View ms
pattern TableCell tc = View tc

instance Pure TableCell ms where
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
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children
        
instance HasAsProp (TableCell ms) where
    type AsProp (TableCell ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tc = tc { as = a }

instance HasAttributesProp (TableCell ms) where
    type Attribute (TableCell ms) = Feature ms
    getAttributes = attributes
    setAttributes as tc = tc { attributes = as }

instance HasChildrenProp (TableCell ms) where
    type Child (TableCell ms) = View ms
    getChildren = children
    setChildren cs tc = tc { children = cs }

instance HasClassesProp (TableCell ms) where
    getClasses = classes
    setClasses cs tc = tc { classes = cs }

instance HasActiveProp (TableCell ms) where
    getActive = active
    setActive a tc = tc { active = a }

instance HasCollapsingProp (TableCell ms) where
    getCollapsing = collapsing
    setCollapsing c tc = tc { collapsing = c }

instance HasDisabledProp (TableCell ms) where
    getDisabled = disabled
    setDisabled d tc = tc { disabled = d }

instance HasErrorProp (TableCell ms) where
    getError = error
    setError e tc = tc { error = e }

instance HasNegativeProp (TableCell ms) where
    getNegative = negative
    setNegative n tc = tc { negative = n }

instance HasPositiveProp (TableCell ms) where
    getPositive = positive
    setPositive p tc = tc { positive = p }

instance HasSelectableProp (TableCell ms) where
    getSelectable = selectable
    setSelectable s tc = tc { selectable = s }

instance HasSingleLineProp (TableCell ms) where
    getSingleLine = singleLine
    setSingleLine sl tc = tc { singleLine = sl }

instance HasTextAlignProp (TableCell ms) where
    getTextAlign = textAlign
    setTextAlign ta tc = tc { textAlign = ta }

instance HasVerticalAlignProp (TableCell ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va tc = tc { verticalAlign = va }

instance HasWarningProp (TableCell ms) where
    getWarning = warning
    setWarning w tc = tc { warning = w }

instance HasWidthProp (TableCell ms) where
    getWidth = width
    setWidth w tc = tc { width = w }


