module Semantic.Elements.Segment (module Semantic.Elements.Segment, module Export) where

import GHC.Generics as G
import Pure.View hiding (color,disabled,textAlign,vertical)

import Semantic.Utils

import Semantic.Elements.Segment.SegmentGroup as Export

import Semantic.Properties.As
import Semantic.Properties.Attached
import Semantic.Properties.Attributes
import Semantic.Properties.Basic
import Semantic.Properties.Children
import Semantic.Properties.Circular
import Semantic.Properties.Classes
import Semantic.Properties.Clearing
import Semantic.Properties.Color
import Semantic.Properties.Compact
import Semantic.Properties.Disabled
import Semantic.Properties.Floated
import Semantic.Properties.Inverted
import Semantic.Properties.Loading
import Semantic.Properties.Padded
import Semantic.Properties.Piled
import Semantic.Properties.Raised
import Semantic.Properties.Secondary
import Semantic.Properties.Size
import Semantic.Properties.Stacked
import Semantic.Properties.Tertiary
import Semantic.Properties.TextAlign
import Semantic.Properties.Vertical

data Segment ms = Segment_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Txt
    , basic :: Bool
    , circular :: Bool
    , clearing :: Bool
    , color :: Txt
    , compact :: Bool
    , disabled :: Bool
    , floated :: Txt
    , inverted :: Bool
    , loading :: Bool
    , padded :: Maybe Txt
    , piled :: Bool
    , raised :: Bool
    , secondary :: Bool
    , size :: Txt
    , stacked :: Bool
    , tertiary :: Bool
    , textAlign :: Txt
    , vertical :: Bool
    } deriving (Generic)

instance Default (Segment ms) where
    def = (G.to gdef) { as = Div }

pattern Segment :: Typeable ms => Segment ms -> View ms
pattern Segment s = View s

instance Typeable ms => Pure Segment ms where
    render Segment_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : basic # "basic"
                : circular # "circular"
                : clearing # "clearing"
                : compact # "compact"
                : disabled # "disabled"
                : inverted # "inverted"
                : loading # "loading"
                : piled # "piled"
                : raised # "raised"
                : secondary # "secondary"
                : stacked # "stacked"
                : tertiary # "tertiary"
                : vertical # "vertical"
                : attached # (attached <>> "attached")
                : may (<>> "padded") padded
                : textAlign
                : floated # ("floated" <<>> floated)
                : "segment"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (Segment ms) where
    type AsProp (Segment ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a s = s { as = a }

instance HasAttachedProp (Segment ms) where
    type AttachedProp (Segment ms) = Txt
    getAttached = attached
    setAttached a s = s { attached = a }

instance HasAttributesProp (Segment ms) where
    type Attribute (Segment ms) = Feature ms
    getAttributes = attributes
    setAttributes as s = s { attributes = as }

instance HasBasicProp (Segment ms) where
    getBasic = basic
    setBasic b s = s { basic = b }

instance HasChildrenProp (Segment ms) where
    type Child (Segment ms) = View ms
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasCircularProp (Segment ms) where
    getCircular = circular
    setCircular c s = s { circular = c }

instance HasClassesProp (Segment ms) where
    getClasses = classes
    setClasses cs s = s { classes = cs }

instance HasClearingProp (Segment ms) where
    getClearing = clearing
    setClearing c s = s { clearing = c }

instance HasColorProp (Segment ms) where
    getColor = color
    setColor c s = s { color = c }

instance HasCompactProp (Segment ms) where
    getCompact = compact
    setCompact c s = s { compact = c }

instance HasDisabledProp (Segment ms) where
    getDisabled = disabled
    setDisabled d s = s { disabled = d }

instance HasFloatedProp (Segment ms) where
    getFloated = floated
    setFloated f s = s { floated = f }

instance HasInvertedProp (Segment ms) where
    getInverted = inverted
    setInverted i s = s { inverted = i }

instance HasLoadingProp (Segment ms) where
    getLoading = loading
    setLoading l s = s { loading = l }

instance HasPaddedProp (Segment ms) where
    getPadded = padded
    setPadded p s = s { padded = p }

instance HasPiledProp (Segment ms) where
    getPiled = piled
    setPiled p s = s { piled = p }

instance HasRaisedProp (Segment ms) where
    getRaised = raised
    setRaised r s = s { raised = r }

instance HasSecondaryProp (Segment ms) where
    getSecondary = secondary
    setSecondary sec s = s { secondary = sec }

instance HasSizeProp (Segment ms) where
    getSize = size
    setSize sz s = s { size = sz }

instance HasStackedProp (Segment ms) where
    getStacked = stacked
    setStacked stkd s = s { stacked = stkd }

instance HasTertiaryProp (Segment ms) where
    getTertiary = tertiary
    setTertiary t s = s { tertiary = t }

instance HasTextAlignProp (Segment ms) where
    getTextAlign = textAlign
    setTextAlign ta s = s { textAlign = ta }

instance HasVerticalProp (Segment ms) where
    getVertical = vertical
    setVertical v s = s { vertical = v }
