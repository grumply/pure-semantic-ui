module Semantic.Collections.Table.TableHeaderCell where

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
  , HasSortedProp(..), pattern Sorted
  , HasTextAlignProp(..), pattern TextAlign
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasWarningProp(..), pattern Warning
  , HasWidthProp(..), pattern Width
  )

import Prelude hiding (error)

data TableHeaderCell ms = TableHeaderCell_
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
    , sorted :: Txt
    , textAlign :: Txt
    , verticalAlign :: Txt
    , warning :: Bool
    , width :: Width
    } deriving (Generic)

instance Default (TableHeaderCell ms) where
    def = (G.to gdef) { as = Th }

pattern TableHeaderCell :: TableHeaderCell ms -> View ms
pattern TableHeaderCell thc = View thc 

instance Pure TableHeaderCell ms where
    render TableHeaderCell_ {..} =
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
                : sorted # (sorted <>> "sorted")
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (TableHeaderCell ms) where
    type AsProp (TableHeaderCell ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a thc = thc { as = a }

instance HasAttributesProp (TableHeaderCell ms) where
    type Attribute (TableHeaderCell ms) = Feature ms
    getAttributes = attributes
    setAttributes as thc = thc { attributes = as }

instance HasChildrenProp (TableHeaderCell ms) where
    type Child (TableHeaderCell ms) = View ms
    getChildren = children
    setChildren cs thc = thc { children = cs }

instance HasClassesProp (TableHeaderCell ms) where
    getClasses = classes
    setClasses cs thc = thc { classes = cs }

instance HasActiveProp (TableHeaderCell ms) where
    getActive = active
    setActive a thc = thc { active = a }

instance HasCollapsingProp (TableHeaderCell ms) where
    getCollapsing = collapsing
    setCollapsing c thc = thc { collapsing = c }

instance HasDisabledProp (TableHeaderCell ms) where
    getDisabled = disabled
    setDisabled d thc = thc { disabled = d }

instance HasErrorProp (TableHeaderCell ms) where
    getError = error
    setError e thc = thc { error = e }

instance HasNegativeProp (TableHeaderCell ms) where
    getNegative = negative
    setNegative n thc = thc { negative = n }

instance HasPositiveProp (TableHeaderCell ms) where
    getPositive = positive
    setPositive p thc = thc { positive = p }

instance HasSelectableProp (TableHeaderCell ms) where
    getSelectable = selectable
    setSelectable s thc = thc { selectable = s }

instance HasSingleLineProp (TableHeaderCell ms) where
    getSingleLine = singleLine
    setSingleLine sl thc = thc { singleLine = sl }

instance HasSortedProp (TableHeaderCell ms) where
    getSorted = sorted
    setSorted s thc = thc { sorted = s }

instance HasTextAlignProp (TableHeaderCell ms) where
    getTextAlign = textAlign
    setTextAlign ta thc = thc { textAlign = ta }

instance HasVerticalAlignProp (TableHeaderCell ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va thc = thc { verticalAlign = va }

instance HasWarningProp (TableHeaderCell ms) where
    getWarning = warning
    setWarning w thc = thc { warning = w }

instance HasWidthProp (TableHeaderCell ms) where
    getWidth = width
    setWidth w thc = thc { width = w }

