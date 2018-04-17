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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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
  )

import Prelude hiding (error)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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

instance HasProp As (Table ms) where
    type Prop As (Table ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a t = t { as = a }

instance HasProp Attributes (Table ms) where
    type Prop Attributes (Table ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as t = t { attributes = as }

instance HasProp Children (Table ms) where
    type Prop Children (Table ms) = [View ms]
    getProp _ = children
    setProp _ cs t = t { children = cs }

instance HasProp Classes (Table ms) where
    type Prop Classes (Table ms) = [Txt]
    getProp _ = classes
    setProp _ cs t = t { classes = cs }

instance HasProp Attached (Table ms) where
    type Prop Attached (Table ms) = Maybe Txt
    getProp _ = attached
    setProp _ a t = t { attached = a }

instance HasProp Basic (Table ms) where
    type Prop Basic (Table ms) = Maybe Txt
    getProp _ = basic
    setProp _ b t = t { basic = b }

instance HasProp Celled (Table ms) where
    type Prop Celled (Table ms) = Bool
    getProp _ = celled
    setProp _ c t = t { celled = c }

instance HasProp Collapsing (Table ms) where
    type Prop Collapsing (Table ms) = Bool
    getProp _ = collapsing
    setProp _ c t = t { collapsing = c }

instance HasProp Color (Table ms) where
    type Prop Color (Table ms) = Txt
    getProp _ = color
    setProp _ c t = t { color = c }

instance HasProp Columns (Table ms) where
    type Prop Columns (Table ms) = Txt
    getProp _ = columns
    setProp _ c t = t { columns = c }

instance HasProp Compact (Table ms) where
    type Prop Compact (Table ms) = Maybe Txt
    getProp _ = compact
    setProp _ c t = t { compact = c }

instance HasProp Definition (Table ms) where
    type Prop Definition (Table ms) = Bool
    getProp _ = definition
    setProp _ d t = t { definition = d }

instance HasProp Fixed (Table ms) where
    type Prop Fixed (Table ms) = Bool
    getProp _ = fixed
    setProp _ f t = t { fixed = f }

instance HasProp Inverted (Table ms) where
    type Prop Inverted (Table ms) = Bool
    getProp _ = inverted
    setProp _ i t = t { inverted = i }

instance HasProp Padded (Table ms) where
    type Prop Padded (Table ms) = Maybe Txt
    getProp _ = padded
    setProp _ p t = t { padded = p }

instance HasProp Selectable (Table ms) where
    type Prop Selectable (Table ms) = Bool
    getProp _ = selectable
    setProp _ s t = t { selectable = s }

instance HasProp SingleLine (Table ms) where
    type Prop SingleLine (Table ms) = Bool
    getProp _ = singleLine
    setProp _ sl t = t { singleLine = sl }

instance HasProp Size (Table ms) where
    type Prop Size (Table ms) = Txt
    getProp _ = size
    setProp _ s t = t { size = s }

instance HasProp Sortable (Table ms) where
    type Prop Sortable (Table ms) = Bool
    getProp _ = sortable
    setProp _ s t = t { sortable = s }

instance HasProp Stackable (Table ms) where
    type Prop Stackable (Table ms) = Bool
    getProp _ = stackable
    setProp _ s t = t { stackable = s }

instance HasProp Striped (Table ms) where
    type Prop Striped (Table ms) = Bool
    getProp _ = striped
    setProp _ s t = t { striped = s }

instance HasProp Structured (Table ms) where
    type Prop Structured (Table ms) = Bool
    getProp _ = structured
    setProp _ s t = t { structured = s }

instance HasProp TextAlign (Table ms) where
    type Prop TextAlign (Table ms) = Txt
    getProp _ = textAlign
    setProp _ ta t = t { textAlign = ta }

instance HasProp Unstackable (Table ms) where
    type Prop Unstackable (Table ms) = Bool
    getProp _ = unstackable
    setProp _ u t = t { unstackable = u }

instance HasProp VerticalAlign (Table ms) where
    type Prop VerticalAlign (Table ms) = Txt
    getProp _ = verticalAlign
    setProp _ va t = t { verticalAlign = va }

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

instance HasProp As (Body ms) where
    type Prop As (Body ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a tb = tb { as = a }

instance HasProp Attributes (Body ms) where
    type Prop Attributes (Body ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as tb = tb { attributes = as }

instance HasProp Children (Body ms) where
    type Prop Children (Body ms) = [View ms]
    getProp _ = children
    setProp _ cs tb = tb { children = cs }

instance HasProp Classes (Body ms) where
    type Prop Classes (Body ms) = [Txt]
    getProp _ = classes
    setProp _ cs tb = tb { classes = cs }

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
    , width :: Txt
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

instance HasProp As (Cell ms) where
    type Prop As (Cell ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a tc = tc { as = a }

instance HasProp Attributes (Cell ms) where
    type Prop Attributes (Cell ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as tc = tc { attributes = as }

instance HasProp Children (Cell ms) where
    type Prop Children (Cell ms) = [View ms]
    getProp _ = children
    setProp _ cs tc = tc { children = cs }

instance HasProp Classes (Cell ms) where
    type Prop Classes (Cell ms) = [Txt]
    getProp _ = classes
    setProp _ cs tc = tc { classes = cs }

instance HasProp Active (Cell ms) where
    type Prop Active (Cell ms) = Bool
    getProp _ = active
    setProp _ a tc = tc { active = a }

instance HasProp Collapsing (Cell ms) where
    type Prop Collapsing (Cell ms) = Bool
    getProp _ = collapsing
    setProp _ c tc = tc { collapsing = c }

instance HasProp Disabled (Cell ms) where
    type Prop Disabled (Cell ms) = Bool
    getProp _ = disabled
    setProp _ d tc = tc { disabled = d }

instance HasProp Error (Cell ms) where
    type Prop Error (Cell ms) = Bool
    getProp _ = error
    setProp _ e tc = tc { error = e }

instance HasProp Negative (Cell ms) where
    type Prop Negative (Cell ms) = Bool
    getProp _ = negative
    setProp _ n tc = tc { negative = n }

instance HasProp Positive (Cell ms) where
    type Prop Positive (Cell ms) = Bool
    getProp _ = positive
    setProp _ p tc = tc { positive = p }

instance HasProp Selectable (Cell ms) where
    type Prop Selectable (Cell ms) = Bool
    getProp _ = selectable
    setProp _ s tc = tc { selectable = s }

instance HasProp SingleLine (Cell ms) where
    type Prop SingleLine (Cell ms) = Bool
    getProp _ = singleLine
    setProp _ sl tc = tc { singleLine = sl }

instance HasProp TextAlign (Cell ms) where
    type Prop TextAlign (Cell ms) = Txt
    getProp _ = textAlign
    setProp _ ta tc = tc { textAlign = ta }

instance HasProp VerticalAlign (Cell ms) where
    type Prop VerticalAlign (Cell ms) = Txt
    getProp _ = verticalAlign
    setProp _ va tc = tc { verticalAlign = va }

instance HasProp Warning (Cell ms) where
    type Prop Warning (Cell ms) = Bool
    getProp _ = warning
    setProp _ w tc = tc { warning = w }

instance HasProp Width (Cell ms) where
    type Prop Width (Cell ms) = Txt
    getProp _ = width
    setProp _ w tc = tc { width = w }

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

instance HasProp As (Footer ms) where
    type Prop As (Footer ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a tf = tf { as = a }

instance HasProp Attributes (Footer ms) where
    type Prop Attributes (Footer ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as tf = tf { attributes = as }

instance HasProp Children (Footer ms) where
    type Prop Children (Footer ms) = [View ms]
    getProp _ = children
    setProp _ cs tf = tf { children = cs }

instance HasProp Classes (Footer ms) where
    type Prop Classes (Footer ms) = [Txt]
    getProp _ = classes
    setProp _ cs tf = tf { classes = cs }

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

instance HasProp As (Header ms) where
    type Prop As (Header ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a th = th { as = a }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as th = th { attributes = as }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs th = th { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs th = th { classes = cs }

instance HasProp FullWidth (Header ms) where
    type Prop FullWidth (Header ms) = Bool
    getProp _ = fullWidth
    setProp _ fw th = th { fullWidth = fw }

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
    , width :: Txt
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

instance HasProp As (HeaderCell ms) where
    type Prop As (HeaderCell ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a thc = thc { as = a }

instance HasProp Attributes (HeaderCell ms) where
    type Prop Attributes (HeaderCell ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as thc = thc { attributes = as }

instance HasProp Children (HeaderCell ms) where
    type Prop Children (HeaderCell ms) = [View ms]
    getProp _ = children
    setProp _ cs thc = thc { children = cs }

instance HasProp Classes (HeaderCell ms) where
    type Prop Classes (HeaderCell ms) = [Txt]
    getProp _ = classes
    setProp _ cs thc = thc { classes = cs }

instance HasProp Active (HeaderCell ms) where
    type Prop Active (HeaderCell ms) = Bool
    getProp _ = active
    setProp _ a thc = thc { active = a }

instance HasProp Collapsing (HeaderCell ms) where
    type Prop Collapsing (HeaderCell ms) = Bool
    getProp _ = collapsing
    setProp _ c thc = thc { collapsing = c }

instance HasProp Disabled (HeaderCell ms) where
    type Prop Disabled (HeaderCell ms) = Bool
    getProp _ = disabled
    setProp _ d thc = thc { disabled = d }

instance HasProp Error (HeaderCell ms) where
    type Prop Error (HeaderCell ms) = Bool
    getProp _ = error
    setProp _ e thc = thc { error = e }

instance HasProp Negative (HeaderCell ms) where
    type Prop Negative (HeaderCell ms) = Bool
    getProp _ = negative
    setProp _ n thc = thc { negative = n }

instance HasProp Positive (HeaderCell ms) where
    type Prop Positive (HeaderCell ms) = Bool
    getProp _ = positive
    setProp _ p thc = thc { positive = p }

instance HasProp Selectable (HeaderCell ms) where
    type Prop Selectable (HeaderCell ms) = Bool
    getProp _ = selectable
    setProp _ s thc = thc { selectable = s }

instance HasProp SingleLine (HeaderCell ms) where
    type Prop SingleLine (HeaderCell ms) = Bool
    getProp _ = singleLine
    setProp _ sl thc = thc { singleLine = sl }

instance HasProp Sorted (HeaderCell ms) where
    type Prop Sorted (HeaderCell ms) = Txt
    getProp _ = sorted
    setProp _ s thc = thc { sorted = s }

instance HasProp TextAlign (HeaderCell ms) where
    type Prop TextAlign (HeaderCell ms) = Txt
    getProp _ = textAlign
    setProp _ ta thc = thc { textAlign = ta }

instance HasProp VerticalAlign (HeaderCell ms) where
    type Prop VerticalAlign (HeaderCell ms) = Txt
    getProp _ = verticalAlign
    setProp _ va thc = thc { verticalAlign = va }

instance HasProp Warning (HeaderCell ms) where
    type Prop Warning (HeaderCell ms) = Bool
    getProp _ = warning
    setProp _ w thc = thc { warning = w }

instance HasProp Width (HeaderCell ms) where
    type Prop Width (HeaderCell ms) = Txt
    getProp _ = width
    setProp _ w thc = thc { width = w }

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

instance HasProp As (Row ms) where
    type Prop As (Row ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a tr = tr { as = a }

instance HasProp Attributes (Row ms) where
    type Prop Attributes (Row ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as tr = tr { attributes = as }

instance HasProp Children (Row ms) where
    type Prop Children (Row ms) = [View ms]
    getProp _ = children
    setProp _ cs tr = tr { children = cs }

instance HasProp Classes (Row ms) where
    type Prop Classes (Row ms) = [Txt]
    getProp _ = classes
    setProp _ cs tr = tr { classes = cs }

instance HasProp Active (Row ms) where
    type Prop Active (Row ms) = Bool
    getProp _ = active
    setProp _ a tr = tr { active = a }

instance HasProp Disabled (Row ms) where
    type Prop Disabled (Row ms) = Bool
    getProp _ = disabled
    setProp _ d tr = tr { disabled = d }

instance HasProp Error (Row ms) where
    type Prop Error (Row ms) = Bool
    getProp _ = error
    setProp _ e tr = tr { error = e }

instance HasProp Negative (Row ms) where
    type Prop Negative (Row ms) = Bool
    getProp _ = negative
    setProp _ n tr = tr { negative = n }

instance HasProp Positive (Row ms) where
    type Prop Positive (Row ms) = Bool
    getProp _ = positive
    setProp _ p tr = tr { positive = p }

instance HasProp TextAlign (Row ms) where
    type Prop TextAlign (Row ms) = Txt
    getProp _ = textAlign
    setProp _ ta tr = tr { textAlign = ta }

instance HasProp VerticalAlign (Row ms) where
    type Prop VerticalAlign (Row ms) = Txt
    getProp _ = verticalAlign
    setProp _ va tr = tr { verticalAlign = va }

instance HasProp Warning (Row ms) where
    type Prop Warning (Row ms) = Bool
    getProp _ = warning
    setProp _ w tr = tr { warning = w }
