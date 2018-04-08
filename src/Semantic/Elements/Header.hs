module Semantic.Elements.Header where

import GHC.Generics as G
import Pure.View hiding (block,color,disabled,textAlign,Header)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon
import Semantic.Elements.Image

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttachedProp(..), pattern Attached
  , HasAttributesProp(..), pattern Attributes
  , HasBlockProp(..), pattern Block
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasColorProp(..), pattern Color
  , HasDisabledProp(..), pattern Disabled
  , HasDividingProp(..), pattern Dividing
  , HasFloatedProp(..), pattern Floated
  , HasInvertedProp(..), pattern Inverted
  , HasSizeProp(..), pattern Size
  , HasSubProp(..), pattern Sub
  , HasTextAlignProp(..), pattern TextAlign
  )

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attached :: Maybe Txt
    , attributes :: [Feature ms]
    , block :: Bool
    , children :: [View ms]
    , classes :: [Txt]
    , color :: Txt
    , disabled :: Bool
    , dividing :: Bool
    , floated :: Txt
    , inverted :: Bool
    , size :: Txt
    , sub :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header h = View h

instance Pure Header ms where
    render Header_ {..} =
        let
            icon = foldPures (\Icon_{} -> const True) False children
            image = foldPures (\Image_{} -> const True) False children

            cs =
                ( "ui"
                : color
                : size
                : block # "block"
                : disabled # "disabled"
                : dividing # "dividing"
                : floated # (floated <<>> "floated")
                : icon # "icon"
                : image # "image"
                : inverted # "inverted"
                : sub # "sub"
                : may (<>> "attached") attached
                : textAlign
                : "header"
                : classes
                )
        in
            as ( mergeClasses $ ClassList cs
               : attributes
               )
               children

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f h = h { as = f }

instance HasAttachedProp (Header ms) where
    type AttachedProp (Header ms) = Maybe Txt
    getAttached = attached
    setAttached attach h = h { attached = attach }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes cs h = h { attributes = cs }

instance HasBlockProp (Header ms) where
    getBlock = block
    setBlock b h = h { block = b }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs h = h { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs h = h { classes = cs }

instance HasColorProp (Header ms) where
    getColor = color
    setColor c h = h { color = c }

instance HasDisabledProp (Header ms) where
    getDisabled = disabled
    setDisabled d h = h { disabled = d }

instance HasDividingProp (Header ms) where
    getDividing = dividing
    setDividing d h = h { dividing = d }

instance HasFloatedProp (Header ms) where
    getFloated = floated
    setFloated f h = h { floated = f }

instance HasInvertedProp (Header ms) where
    getInverted = inverted
    setInverted i h = h { inverted = i }

instance HasSizeProp (Header ms) where
    getSize = size
    setSize s h = h { size = s }

instance HasSubProp (Header ms) where
    getSub = sub
    setSub s h = h { sub = s }

instance HasTextAlignProp (Header ms) where
    getTextAlign = textAlign
    setTextAlign ta h = h { textAlign = ta }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content hc = View hc

instance Pure Content ms where
    render Content_ {..} =
        as
            ( ClassList ( "content" : classes)
            : attributes
            )
            children

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f hc = hc { as = f }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes cs hc = hc { attributes = cs }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs hc = hc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs hc = hc { classes = cs }

data Subheader ms = Subheader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Subheader ms) where
    def = (G.to gdef) { as = Div }

pattern Subheader :: Subheader ms -> View ms
pattern Subheader hs = View hs

instance Pure Subheader ms where
    render Subheader_ {..} =
        as
            ( ClassList ( "sub" : "header" : classes )
            : attributes
            )
            children

instance HasAsProp (Subheader ms) where
    type AsProp (Subheader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f hs = hs { as = f }

instance HasAttributesProp (Subheader ms) where
    type Attribute (Subheader ms) = Feature ms
    getAttributes = attributes
    setAttributes cs hs = hs { attributes = cs }

instance HasChildrenProp (Subheader ms) where
    type Child (Subheader ms) = View ms
    getChildren = children
    setChildren cs hs = hs { children = cs }

instance HasClassesProp (Subheader ms) where
    getClasses = classes
    setClasses cs hs = hs { classes = cs }
