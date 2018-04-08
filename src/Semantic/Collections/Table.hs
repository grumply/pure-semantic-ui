module Semantic.Collections.Table
  ( module Properties
  , module Tools
  , Table(..), pattern Table
  , Body(..), pattern Body
  , Cell(..), pattern Cell
  , Footer(..), pattern Footer
  , Header(..), pattern Header
  , HeaderCell(..), pattern HeaderCell
  , Row(..), pattern Row
  ) where

import GHC.Generics as G
import Pure.View hiding (color,fixed,textAlign,verticalAlign,Table,Body,Header,Footer,disabled,active,width)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasAttachedProp(..), pattern Attached
  , HasBasicProp(..), pattern Basic
  , HasCelledProp(..), pattern Celled
  , HasCollapsingProp(..), pattern Collapsing
  , HasColorProp(..), pattern Color
  , HasColumnsProp(..), pattern Columns
  , HasCompactProp(..), pattern Compact
  , HasDefinitionProp(..), pattern Definition
  , HasFixedProp(..), pattern Fixed
  , HasInvertedProp(..), pattern Inverted
  , HasPaddedProp(..), pattern Padded
  , HasSelectableProp(..), pattern Selectable
  , HasSingleLineProp(..), pattern SingleLine
  , HasSizeProp(..), pattern Size
  , HasSortableProp(..), pattern Sortable
  , HasStackableProp(..), pattern Stackable
  , HasStripedProp(..), pattern Striped
  , HasStructuredProp(..), pattern Structured
  , HasTextAlignProp(..), pattern TextAlign
  , HasUnstackableProp(..), pattern Unstackable
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasActiveProp(..), pattern Active
  , HasDisabledProp(..), pattern Disabled
  , HasErrorProp(..), pattern Error
  , HasNegativeProp(..), pattern Negative
  , HasPositiveProp(..), pattern Positive
  , HasWarningProp(..), pattern Warning
  , HasWidthProp(..), pattern Width
  , HasFullWidthProp(..), pattern FullWidth
  , HasSortedProp(..), pattern Sorted
  )

import Prelude hiding (error)

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

pattern Table :: Table ms -> View ms
pattern Table t = View t

instance Pure Table ms where
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
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Table ms) where
    type AsProp (Table ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a t = t { as = a }

instance HasAttributesProp (Table ms) where
    type Attribute (Table ms) = Feature ms
    getAttributes = attributes
    setAttributes as t = t { attributes = as }

instance HasChildrenProp (Table ms) where
    type Child (Table ms) = View ms
    getChildren = children
    setChildren cs t = t { children = cs }

instance HasClassesProp (Table ms) where
    getClasses = classes
    setClasses cs t = t { classes = cs }

instance HasAttachedProp (Table ms) where
    type AttachedProp (Table ms) = Maybe Txt
    getAttached = attached
    setAttached a t = t { attached = a }

instance HasBasicProp (Table ms) where
    type BasicProp (Table ms) = Maybe Txt
    getBasic = basic
    setBasic b t = t { basic = b }

instance HasCelledProp (Table ms) where
    getCelled = celled
    setCelled c t = t { celled = c }

instance HasCollapsingProp (Table ms) where
    getCollapsing = collapsing
    setCollapsing c t = t { collapsing = c }

instance HasColorProp (Table ms) where
    getColor = color
    setColor c t = t { color = c }

instance HasColumnsProp (Table ms) where
    getColumns = columns
    setColumns c t = t { columns = c }

instance HasCompactProp (Table ms) where
    type CompactProp (Table ms) = Maybe Txt
    getCompact = compact
    setCompact c t = t { compact = c }

instance HasDefinitionProp (Table ms) where
    getDefinition = definition
    setDefinition d t = t { definition = d }

instance HasFixedProp (Table ms) where
    type FixedProp (Table ms) = Bool
    getFixed = fixed
    setFixed f t = t { fixed = f }

instance HasInvertedProp (Table ms) where
    getInverted = inverted
    setInverted i t = t { inverted = i }

instance HasPaddedProp (Table ms) where
    getPadded = padded
    setPadded p t = t { padded = p }

instance HasSelectableProp (Table ms) where
    getSelectable = selectable
    setSelectable s t = t { selectable = s }

instance HasSingleLineProp (Table ms) where
    getSingleLine = singleLine
    setSingleLine sl t = t { singleLine = sl }

instance HasSizeProp (Table ms) where
    getSize = size
    setSize s t = t { size = s }

instance HasSortableProp (Table ms) where
    getSortable = sortable
    setSortable s t = t { sortable = s }

instance HasStackableProp (Table ms) where
    type StackableProp (Table ms) = Bool
    getStackable = stackable
    setStackable s t = t { stackable = s }

instance HasStripedProp (Table ms) where
    getStriped = striped
    setStriped s t = t { striped = s }

instance HasStructuredProp (Table ms) where
    getStructured = structured
    setStructured s t = t { structured = s }

instance HasTextAlignProp (Table ms) where
    getTextAlign = textAlign
    setTextAlign ta t = t { textAlign = ta }

instance HasUnstackableProp (Table ms) where
    getUnstackable = unstackable
    setUnstackable u t = t { unstackable = u }

instance HasVerticalAlignProp (Table ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va t = t { verticalAlign = va }

data Body ms = Body_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Body ms) where
    def = (G.to gdef) { as = Tbody }

pattern Body :: Body ms -> View ms
pattern Body tb = View tb

instance Pure Body ms where
    render Body_ {..} =
        as
            ( ClassList classes
            : attributes
            )
            children

instance HasAsProp (Body ms) where
    type AsProp (Body ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tb = tb { as = a }

instance HasAttributesProp (Body ms) where
    type Attribute (Body ms) = Feature ms
    getAttributes = attributes
    setAttributes as tb = tb { attributes = as }

instance HasChildrenProp (Body ms) where
    type Child (Body ms) = View ms
    getChildren = children
    setChildren cs tb = tb { children = cs }

instance HasClassesProp (Body ms) where
    getClasses = classes
    setClasses cs tb = tb { classes = cs }

data Cell ms = Cell_
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

instance Default (Cell ms) where
    def = (G.to gdef) { as = Td }

pattern Cell :: Cell ms -> View ms
pattern Cell tc = View tc

instance Pure Cell ms where
    render Cell_ {..} =
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

instance HasAsProp (Cell ms) where
    type AsProp (Cell ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tc = tc { as = a }

instance HasAttributesProp (Cell ms) where
    type Attribute (Cell ms) = Feature ms
    getAttributes = attributes
    setAttributes as tc = tc { attributes = as }

instance HasChildrenProp (Cell ms) where
    type Child (Cell ms) = View ms
    getChildren = children
    setChildren cs tc = tc { children = cs }

instance HasClassesProp (Cell ms) where
    getClasses = classes
    setClasses cs tc = tc { classes = cs }

instance HasActiveProp (Cell ms) where
    getActive = active
    setActive a tc = tc { active = a }

instance HasCollapsingProp (Cell ms) where
    getCollapsing = collapsing
    setCollapsing c tc = tc { collapsing = c }

instance HasDisabledProp (Cell ms) where
    getDisabled = disabled
    setDisabled d tc = tc { disabled = d }

instance HasErrorProp (Cell ms) where
    getError = error
    setError e tc = tc { error = e }

instance HasNegativeProp (Cell ms) where
    getNegative = negative
    setNegative n tc = tc { negative = n }

instance HasPositiveProp (Cell ms) where
    getPositive = positive
    setPositive p tc = tc { positive = p }

instance HasSelectableProp (Cell ms) where
    getSelectable = selectable
    setSelectable s tc = tc { selectable = s }

instance HasSingleLineProp (Cell ms) where
    getSingleLine = singleLine
    setSingleLine sl tc = tc { singleLine = sl }

instance HasTextAlignProp (Cell ms) where
    getTextAlign = textAlign
    setTextAlign ta tc = tc { textAlign = ta }

instance HasVerticalAlignProp (Cell ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va tc = tc { verticalAlign = va }

instance HasWarningProp (Cell ms) where
    getWarning = warning
    setWarning w tc = tc { warning = w }

instance HasWidthProp (Cell ms) where
    getWidth = width
    setWidth w tc = tc { width = w }

data Footer ms = Footer_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Footer ms) where
    def = (G.to gdef) { as = Tfoot }

pattern Footer :: Footer ms -> View ms
pattern Footer tf = View tf

instance Pure Footer ms where
    render Footer_ {..} =
        as
            ( ClassList classes
            : attributes
            )
            children

instance HasAsProp (Footer ms) where
    type AsProp (Footer ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tf = tf { as = a }

instance HasAttributesProp (Footer ms) where
    type Attribute (Footer ms) = Feature ms
    getAttributes = attributes
    setAttributes as tf = tf { attributes = as }

instance HasChildrenProp (Footer ms) where
    type Child (Footer ms) = View ms
    getChildren = children
    setChildren cs tf = tf { children = cs }

instance HasClassesProp (Footer ms) where
    getClasses = classes
    setClasses cs tf = tf { classes = cs }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fullWidth :: Bool
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Thead }

pattern Header :: Header ms -> View ms
pattern Header th = View th

instance Pure Header ms where
    render Header_ {..} =
        let
            cs =
                ( fullWidth # "full-width"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a th = th { as = a }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes as th = th { attributes = as }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs th = th { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs th = th { classes = cs }

instance HasFullWidthProp (Header ms) where
    getFullWidth = fullWidth
    setFullWidth fw th = th { fullWidth = fw }
data HeaderCell ms = HeaderCell_
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

instance Default (HeaderCell ms) where
    def = (G.to gdef) { as = Th }

pattern HeaderCell :: HeaderCell ms -> View ms
pattern HeaderCell thc = View thc

instance Pure HeaderCell ms where
    render HeaderCell_ {..} =
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

instance HasAsProp (HeaderCell ms) where
    type AsProp (HeaderCell ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a thc = thc { as = a }

instance HasAttributesProp (HeaderCell ms) where
    type Attribute (HeaderCell ms) = Feature ms
    getAttributes = attributes
    setAttributes as thc = thc { attributes = as }

instance HasChildrenProp (HeaderCell ms) where
    type Child (HeaderCell ms) = View ms
    getChildren = children
    setChildren cs thc = thc { children = cs }

instance HasClassesProp (HeaderCell ms) where
    getClasses = classes
    setClasses cs thc = thc { classes = cs }

instance HasActiveProp (HeaderCell ms) where
    getActive = active
    setActive a thc = thc { active = a }

instance HasCollapsingProp (HeaderCell ms) where
    getCollapsing = collapsing
    setCollapsing c thc = thc { collapsing = c }

instance HasDisabledProp (HeaderCell ms) where
    getDisabled = disabled
    setDisabled d thc = thc { disabled = d }

instance HasErrorProp (HeaderCell ms) where
    getError = error
    setError e thc = thc { error = e }

instance HasNegativeProp (HeaderCell ms) where
    getNegative = negative
    setNegative n thc = thc { negative = n }

instance HasPositiveProp (HeaderCell ms) where
    getPositive = positive
    setPositive p thc = thc { positive = p }

instance HasSelectableProp (HeaderCell ms) where
    getSelectable = selectable
    setSelectable s thc = thc { selectable = s }

instance HasSingleLineProp (HeaderCell ms) where
    getSingleLine = singleLine
    setSingleLine sl thc = thc { singleLine = sl }

instance HasSortedProp (HeaderCell ms) where
    getSorted = sorted
    setSorted s thc = thc { sorted = s }

instance HasTextAlignProp (HeaderCell ms) where
    getTextAlign = textAlign
    setTextAlign ta thc = thc { textAlign = ta }

instance HasVerticalAlignProp (HeaderCell ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va thc = thc { verticalAlign = va }

instance HasWarningProp (HeaderCell ms) where
    getWarning = warning
    setWarning w thc = thc { warning = w }

instance HasWidthProp (HeaderCell ms) where
    getWidth = width
    setWidth w thc = thc { width = w }

data Row ms = Row_
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

instance Default (Row ms) where
    def = (G.to gdef) { as = Tr }

pattern Row :: Row ms -> View ms
pattern Row tr = View tr

instance Pure Row ms where
    render Row_ {..} =
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

instance HasAsProp (Row ms) where
    type AsProp (Row ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a tr = tr { as = a }

instance HasAttributesProp (Row ms) where
    type Attribute (Row ms) = Feature ms
    getAttributes = attributes
    setAttributes as tr = tr { attributes = as }

instance HasChildrenProp (Row ms) where
    type Child (Row ms) = View ms
    getChildren = children
    setChildren cs tr = tr { children = cs }

instance HasClassesProp (Row ms) where
    getClasses = classes
    setClasses cs tr = tr { classes = cs }

instance HasActiveProp (Row ms) where
    getActive = active
    setActive a tr = tr { active = a }

instance HasDisabledProp (Row ms) where
    getDisabled = disabled
    setDisabled d tr = tr { disabled = d }

instance HasErrorProp (Row ms) where
    getError = error
    setError e tr = tr { error = e }

instance HasNegativeProp (Row ms) where
    getNegative = negative
    setNegative n tr = tr { negative = n }

instance HasPositiveProp (Row ms) where
    getPositive = positive
    setPositive p tr = tr { positive = p }

instance HasTextAlignProp (Row ms) where
    getTextAlign = textAlign
    setTextAlign ta tr = tr { textAlign = ta }

instance HasVerticalAlignProp (Row ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va tr = tr { verticalAlign = va }

instance HasWarningProp (Row ms) where
    getWarning = warning
    setWarning w tr = tr { warning = w }
