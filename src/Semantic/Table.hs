module Semantic.Table
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

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Attached, Attached(..)
  , pattern Basic, Basic(..)
  , pattern Celled, Celled(..)
  , pattern Collapsing, Collapsing(..)
  , pattern Color, Color(..)
  , pattern Columns, Columns(..)
  , pattern Compact, Compact(..)
  , pattern Definition, Definition(..)
  , pattern Fixed, Fixed(..)
  , pattern Inverted, Inverted(..)
  , pattern Padded, Padded(..)
  , pattern Selectable, Selectable(..)
  , pattern SingleLine, SingleLine(..)
  , pattern Size, Size(..)
  , pattern Sortable, Sortable(..)
  , pattern Stackable, Stackable(..)
  , pattern Striped, Striped(..)
  , pattern Structured, Structured(..)
  , pattern TextAlign, TextAlign(..)
  , pattern Unstackable, Unstackable(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Active, Active(..)
  , pattern Disabled, Disabled(..)
  , pattern Error, Error(..)
  , pattern Negative, Negative(..)
  , pattern Positive, Positive(..)
  , pattern Warning, Warning(..)
  , pattern Width, Width(..)
  , pattern FullWidth, FullWidth(..)
  , pattern Sorted, Sorted(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Prelude hiding (error)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Table = Table_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , attached :: Maybe Txt
    , basic :: Maybe Txt
    , celled :: Bool
    , collapsing :: Bool
    , color :: Txt
    , columns :: Txt
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

instance Default Table where
    def = (G.to gdef) { as = \fs cs -> HTML.Table & Features fs & Children cs }

pattern Table :: Table -> Table
pattern Table t = t

instance Pure Table where
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
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Table where
    type Prop As Table = Features -> [View] -> View
    getProp _ = as
    setProp _ a t = t { as = a }

instance HasFeatures Table where
    getFeatures = features
    setFeatures as t = t { features = as }

instance HasChildren Table where
    getChildren = children
    setChildren cs t = t { children = cs }


instance HasProp Attached Table where
    type Prop Attached Table = Maybe Txt
    getProp _ = attached
    setProp _ a t = t { attached = a }

instance HasProp Basic Table where
    type Prop Basic Table = Maybe Txt
    getProp _ = basic
    setProp _ b t = t { basic = b }

instance HasProp Celled Table where
    type Prop Celled Table = Bool
    getProp _ = celled
    setProp _ c t = t { celled = c }

instance HasProp Collapsing Table where
    type Prop Collapsing Table = Bool
    getProp _ = collapsing
    setProp _ c t = t { collapsing = c }

instance HasProp Color Table where
    type Prop Color Table = Txt
    getProp _ = color
    setProp _ c t = t { color = c }

instance HasProp Columns Table where
    type Prop Columns Table = Txt
    getProp _ = columns
    setProp _ c t = t { columns = c }

instance HasProp Compact Table where
    type Prop Compact Table = Maybe Txt
    getProp _ = compact
    setProp _ c t = t { compact = c }

instance HasProp Definition Table where
    type Prop Definition Table = Bool
    getProp _ = definition
    setProp _ d t = t { definition = d }

instance HasProp Fixed Table where
    type Prop Fixed Table = Bool
    getProp _ = fixed
    setProp _ f t = t { fixed = f }

instance HasProp Inverted Table where
    type Prop Inverted Table = Bool
    getProp _ = inverted
    setProp _ i t = t { inverted = i }

instance HasProp Padded Table where
    type Prop Padded Table = Maybe Txt
    getProp _ = padded
    setProp _ p t = t { padded = p }

instance HasProp Selectable Table where
    type Prop Selectable Table = Bool
    getProp _ = selectable
    setProp _ s t = t { selectable = s }

instance HasProp SingleLine Table where
    type Prop SingleLine Table = Bool
    getProp _ = singleLine
    setProp _ sl t = t { singleLine = sl }

instance HasProp Size Table where
    type Prop Size Table = Txt
    getProp _ = size
    setProp _ s t = t { size = s }

instance HasProp Sortable Table where
    type Prop Sortable Table = Bool
    getProp _ = sortable
    setProp _ s t = t { sortable = s }

instance HasProp Stackable Table where
    type Prop Stackable Table = Bool
    getProp _ = stackable
    setProp _ s t = t { stackable = s }

instance HasProp Striped Table where
    type Prop Striped Table = Bool
    getProp _ = striped
    setProp _ s t = t { striped = s }

instance HasProp Structured Table where
    type Prop Structured Table = Bool
    getProp _ = structured
    setProp _ s t = t { structured = s }

instance HasProp TextAlign Table where
    type Prop TextAlign Table = Txt
    getProp _ = textAlign
    setProp _ ta t = t { textAlign = ta }

instance HasProp Unstackable Table where
    type Prop Unstackable Table = Bool
    getProp _ = unstackable
    setProp _ u t = t { unstackable = u }

instance HasProp VerticalAlign Table where
    type Prop VerticalAlign Table = Txt
    getProp _ = verticalAlign
    setProp _ va t = t { verticalAlign = va }

data Body = Body_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Body where
    def = (G.to gdef) { as = \fs cs -> Tbody & Features fs & Children cs }

pattern Body :: Body -> Body
pattern Body tb = tb

instance Pure Body where
    render Body_ {..} =
        as
            : attributes
            )
            children

instance HasProp As Body where
    type Prop As Body = Features -> [View] -> View
    getProp _ = as
    setProp _ a tb = tb { as = a }

instance HasFeatures Body where
    getFeatures = features
    setFeatures as tb = tb { features = as }

instance HasChildren Body where
    getChildren = children
    setChildren cs tb = tb { children = cs }


data Cell = Cell_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
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
    , width :: Txt
    } deriving (Generic)

instance Default Cell where
    def = (G.to gdef) { as = \fs cs -> Td & Features fs & Children cs }

pattern Cell :: Cell -> Cell
pattern Cell tc = tc

instance Pure Cell where
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
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Cell where
    type Prop As Cell = Features -> [View] -> View
    getProp _ = as
    setProp _ a tc = tc { as = a }

instance HasFeatures Cell where
    getFeatures = features
    setFeatures as tc = tc { features = as }

instance HasChildren Cell where
    getChildren = children
    setChildren cs tc = tc { children = cs }


instance HasProp Active Cell where
    type Prop Active Cell = Bool
    getProp _ = active
    setProp _ a tc = tc { active = a }

instance HasProp Collapsing Cell where
    type Prop Collapsing Cell = Bool
    getProp _ = collapsing
    setProp _ c tc = tc { collapsing = c }

instance HasProp Disabled Cell where
    type Prop Disabled Cell = Bool
    getProp _ = disabled
    setProp _ d tc = tc { disabled = d }

instance HasProp Error Cell where
    type Prop Error Cell = Bool
    getProp _ = error
    setProp _ e tc = tc { error = e }

instance HasProp Negative Cell where
    type Prop Negative Cell = Bool
    getProp _ = negative
    setProp _ n tc = tc { negative = n }

instance HasProp Positive Cell where
    type Prop Positive Cell = Bool
    getProp _ = positive
    setProp _ p tc = tc { positive = p }

instance HasProp Selectable Cell where
    type Prop Selectable Cell = Bool
    getProp _ = selectable
    setProp _ s tc = tc { selectable = s }

instance HasProp SingleLine Cell where
    type Prop SingleLine Cell = Bool
    getProp _ = singleLine
    setProp _ sl tc = tc { singleLine = sl }

instance HasProp TextAlign Cell where
    type Prop TextAlign Cell = Txt
    getProp _ = textAlign
    setProp _ ta tc = tc { textAlign = ta }

instance HasProp VerticalAlign Cell where
    type Prop VerticalAlign Cell = Txt
    getProp _ = verticalAlign
    setProp _ va tc = tc { verticalAlign = va }

instance HasProp Warning Cell where
    type Prop Warning Cell = Bool
    getProp _ = warning
    setProp _ w tc = tc { warning = w }

instance HasProp Width Cell where
    type Prop Width Cell = Txt
    getProp _ = width
    setProp _ w tc = tc { width = w }

data Footer = Footer_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Footer where
    def = (G.to gdef) { as = \fs cs -> Tfoot & Features fs & Children cs }

pattern Footer :: Footer -> Footer
pattern Footer tf = tf

instance Pure Footer where
    render Footer_ {..} =
        as
            : attributes
            )
            children

instance HasProp As Footer where
    type Prop As Footer = Features -> [View] -> View
    getProp _ = as
    setProp _ a tf = tf { as = a }

instance HasFeatures Footer where
    getFeatures = features
    setFeatures as tf = tf { features = as }

instance HasChildren Footer where
    getChildren = children
    setChildren cs tf = tf { children = cs }


data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , fullWidth :: Bool
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Thead & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header th = th

instance Pure Header where
    render Header_ {..} =
        let
            cs =
                ( fullWidth # "full-width"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ a th = th { as = a }

instance HasFeatures Header where
    getFeatures = features
    setFeatures as th = th { features = as }

instance HasChildren Header where
    getChildren = children
    setChildren cs th = th { children = cs }


instance HasProp FullWidth Header where
    type Prop FullWidth Header = Bool
    getProp _ = fullWidth
    setProp _ fw th = th { fullWidth = fw }

data HeaderCell = HeaderCell_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
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
    , width :: Txt
    } deriving (Generic)

instance Default HeaderCell where
    def = (G.to gdef) { as = \fs cs -> Th & Features fs & Children cs }

pattern HeaderCell :: HeaderCell -> HeaderCell
pattern HeaderCell thc = thc

instance Pure HeaderCell where
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
                )
        in
            as
                : attributes
                )
                children

instance HasProp As HeaderCell where
    type Prop As HeaderCell = Features -> [View] -> View
    getProp _ = as
    setProp _ a thc = thc { as = a }

instance HasFeatures HeaderCell where
    getFeatures = features
    setFeatures as thc = thc { features = as }

instance HasChildren HeaderCell where
    getChildren = children
    setChildren cs thc = thc { children = cs }


instance HasProp Active HeaderCell where
    type Prop Active HeaderCell = Bool
    getProp _ = active
    setProp _ a thc = thc { active = a }

instance HasProp Collapsing HeaderCell where
    type Prop Collapsing HeaderCell = Bool
    getProp _ = collapsing
    setProp _ c thc = thc { collapsing = c }

instance HasProp Disabled HeaderCell where
    type Prop Disabled HeaderCell = Bool
    getProp _ = disabled
    setProp _ d thc = thc { disabled = d }

instance HasProp Error HeaderCell where
    type Prop Error HeaderCell = Bool
    getProp _ = error
    setProp _ e thc = thc { error = e }

instance HasProp Negative HeaderCell where
    type Prop Negative HeaderCell = Bool
    getProp _ = negative
    setProp _ n thc = thc { negative = n }

instance HasProp Positive HeaderCell where
    type Prop Positive HeaderCell = Bool
    getProp _ = positive
    setProp _ p thc = thc { positive = p }

instance HasProp Selectable HeaderCell where
    type Prop Selectable HeaderCell = Bool
    getProp _ = selectable
    setProp _ s thc = thc { selectable = s }

instance HasProp SingleLine HeaderCell where
    type Prop SingleLine HeaderCell = Bool
    getProp _ = singleLine
    setProp _ sl thc = thc { singleLine = sl }

instance HasProp Sorted HeaderCell where
    type Prop Sorted HeaderCell = Txt
    getProp _ = sorted
    setProp _ s thc = thc { sorted = s }

instance HasProp TextAlign HeaderCell where
    type Prop TextAlign HeaderCell = Txt
    getProp _ = textAlign
    setProp _ ta thc = thc { textAlign = ta }

instance HasProp VerticalAlign HeaderCell where
    type Prop VerticalAlign HeaderCell = Txt
    getProp _ = verticalAlign
    setProp _ va thc = thc { verticalAlign = va }

instance HasProp Warning HeaderCell where
    type Prop Warning HeaderCell = Bool
    getProp _ = warning
    setProp _ w thc = thc { warning = w }

instance HasProp Width HeaderCell where
    type Prop Width HeaderCell = Txt
    getProp _ = width
    setProp _ w thc = thc { width = w }

data Row = Row_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , disabled :: Bool
    , error :: Bool
    , negative :: Bool
    , positive :: Bool
    , textAlign :: Txt
    , verticalAlign :: Txt
    , warning :: Bool
    } deriving (Generic)

instance Default Row where
    def = (G.to gdef) { as = \fs cs -> Tr & Features fs & Children cs }

pattern Row :: Row -> Row
pattern Row tr = tr

instance Pure Row where
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
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Row where
    type Prop As Row = Features -> [View] -> View
    getProp _ = as
    setProp _ a tr = tr { as = a }

instance HasFeatures Row where
    getFeatures = features
    setFeatures as tr = tr { features = as }

instance HasChildren Row where
    getChildren = children
    setChildren cs tr = tr { children = cs }


instance HasProp Active Row where
    type Prop Active Row = Bool
    getProp _ = active
    setProp _ a tr = tr { active = a }

instance HasProp Disabled Row where
    type Prop Disabled Row = Bool
    getProp _ = disabled
    setProp _ d tr = tr { disabled = d }

instance HasProp Error Row where
    type Prop Error Row = Bool
    getProp _ = error
    setProp _ e tr = tr { error = e }

instance HasProp Negative Row where
    type Prop Negative Row = Bool
    getProp _ = negative
    setProp _ n tr = tr { negative = n }

instance HasProp Positive Row where
    type Prop Positive Row = Bool
    getProp _ = positive
    setProp _ p tr = tr { positive = p }

instance HasProp TextAlign Row where
    type Prop TextAlign Row = Txt
    getProp _ = textAlign
    setProp _ ta tr = tr { textAlign = ta }

instance HasProp VerticalAlign Row where
    type Prop VerticalAlign Row = Txt
    getProp _ = verticalAlign
    setProp _ va tr = tr { verticalAlign = va }

instance HasProp Warning Row where
    type Prop Warning Row = Bool
    getProp _ = warning
    setProp _ w tr = tr { warning = w }
