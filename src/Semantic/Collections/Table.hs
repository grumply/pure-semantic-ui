module Semantic.Collections.Table (module Semantic.Collections.Table, module Export) where

import GHC.Generics as G
import Pure.View hiding (color,fixed,textAlign,verticalAlign,Table)
import qualified Pure.View as HTML

import Semantic.Utils

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
  )

import Semantic.Collections.Table.TableBody as Export
import Semantic.Collections.Table.TableCell as Export
import Semantic.Collections.Table.TableFooter as Export
import Semantic.Collections.Table.TableHeader as Export
import Semantic.Collections.Table.TableHeaderCell as Export
import Semantic.Collections.Table.TableRow as Export

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

