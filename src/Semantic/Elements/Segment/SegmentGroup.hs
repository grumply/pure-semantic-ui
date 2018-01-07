module Semantic.Elements.Segment.SegmentGroup where

import GHC.Generics as G
import Pure.View hiding (color,disabled,horizontal,textAlign,vertical)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Compact
import Semantic.Properties.Horizontal
import Semantic.Properties.Piled
import Semantic.Properties.Raised
import Semantic.Properties.Size
import Semantic.Properties.Stacked

data SegmentGroup ms = SegmentGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , compact :: Bool
    , horizontal :: Bool
    , piled :: Bool
    , raised :: Bool
    , size :: Txt
    , stacked :: Bool
    } deriving (Generic)

instance Default (SegmentGroup ms) where
    def = (G.to gdef) { as = Div }

pattern SegmentGroup :: SegmentGroup ms -> View ms
pattern SegmentGroup sg = View sg

instance Pure SegmentGroup ms where
    render SegmentGroup_ {..} =
        let
            cs =
                ( "ui"
                : size
                : compact # "compact"
                : horizontal # "horizontal"
                : piled # "piled"
                : raised # "raised"
                : stacked # "stacked"
                : "segments"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes 
                )
                children

instance HasAsProp (SegmentGroup ms) where
    type AsProp (SegmentGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sg = sg { as = a }

instance HasAttributesProp (SegmentGroup ms) where
    type Attribute (SegmentGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as sg = sg { attributes = as }

instance HasChildrenProp (SegmentGroup ms) where
    type Child (SegmentGroup ms) = View ms
    getChildren = children
    setChildren cs sg = sg { children = cs }

instance HasClassesProp (SegmentGroup ms) where
    getClasses = classes
    setClasses cs sg = sg { classes = cs }

instance HasCompactProp (SegmentGroup ms) where
    getCompact = compact
    setCompact c sg = sg { compact = c }

instance HasHorizontalProp (SegmentGroup ms) where
    getHorizontal = horizontal
    setHorizontal h sg = sg { horizontal = h }

instance HasPiledProp (SegmentGroup ms) where
    getPiled = piled
    setPiled p sg = sg { piled = p }

instance HasRaisedProp (SegmentGroup ms) where
    getRaised = raised
    setRaised r sg = sg { raised = r }

instance HasSizeProp (SegmentGroup ms) where
    getSize = size
    setSize s sg = sg { size = s }

instance HasStackedProp (SegmentGroup ms) where
    getStacked = stacked
    setStacked s sg = sg { stacked = s }