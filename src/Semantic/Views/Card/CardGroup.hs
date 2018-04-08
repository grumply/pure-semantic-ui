module Semantic.Views.Card.CardGroup where

import GHC.Generics as G
import Pure.View hiding (textAlign)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasDoublingProp(..), pattern Doubling
  , HasItemsPerRowProp(..), pattern ItemsPerRow
  , HasStackableProp(..), pattern Stackable
  , HasTextAlignProp(..), pattern TextAlign
  )

data CardGroup ms = CardGroup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , doubling :: Bool
    , itemsPerRow :: Width
    , stackable :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (CardGroup ms) where
    def = (G.to gdef) { as = Div }

pattern CardGroup :: CardGroup ms -> View ms
pattern CardGroup cg = View cg

instance Pure CardGroup ms where
    render CardGroup_ {..} =
        let
            cs =
                ( "ui"
                : doubling # "doubling"
                : stackable # "stackable"
                : textAlign
                : widthProp def width def
                : "cards"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CardGroup ms) where
    type AsProp (CardGroup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cg = cg { as = a }

instance HasAttributesProp (CardGroup ms) where
    type Attribute (CardGroup ms) = Feature ms
    getAttributes = attributes
    setAttributes as cg = cg { attributes = as }

instance HasChildrenProp (CardGroup ms) where
    type Child (CardGroup ms) = View ms
    getChildren = children
    setChildren cs cg = cg { children = cs }

instance HasClassesProp (CardGroup ms) where
    getClasses = classes
    setClasses cs cg = cg { classes = cs }

instance HasDoublingProp (CardGroup ms) where
    getDoubling = doubling
    setDoubling d cg = cg { doubling = d }

instance HasItemsPerRowProp (CardGroup ms) where
    getItemsPerRow = itemsPerRow
    setItemsPerRow ipr cg = cg { itemsPerRow = ipr }

instance HasStackableProp (CardGroup ms) where
    type StackableProp (CardGroup ms) = Bool
    getStackable = stackable
    setStackable s cg = cg { stackable = s }

instance HasTextAlignProp (CardGroup ms) where
    getTextAlign = textAlign
    setTextAlign ta cc = cc { textAlign = ta }
