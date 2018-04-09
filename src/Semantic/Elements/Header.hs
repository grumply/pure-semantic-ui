module Semantic.Elements.Header
  ( module Properties
  , module Tools
  , Header(..), pattern Header
  , Content(..), pattern Content
  , Subheader(..), pattern Subheader
  ) where

import GHC.Generics as G
import Pure.View hiding (block,color,disabled,textAlign,Header,Content)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon
import Semantic.Elements.Image

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Attributes, Attributes(..)
  , pattern Block, Block(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Color, Color(..)
  , pattern Disabled, Disabled(..)
  , pattern Dividing, Dividing(..)
  , pattern Floated, Floated(..)
  , pattern Inverted, Inverted(..)
  , pattern Size, Size(..)
  , pattern Sub, Sub(..)
  , pattern TextAlign, TextAlign(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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

instance HasProp As (Header ms) where
    type Prop As (Header ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f h = h { as = f }

instance HasProp Attached (Header ms) where
    type Prop Attached (Header ms) = Maybe Txt
    getProp _ = attached
    setProp _ attach h = h { attached = attach }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs h = h { attributes = cs }

instance HasProp Block (Header ms) where
    type Prop Block (Header ms) = Bool
    getProp _ = block
    setProp _ b h = h { block = b }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs h = h { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs h = h { classes = cs }

instance HasProp Color (Header ms) where
    type Prop Color (Header ms) = Txt
    getProp _ = color
    setProp _ c h = h { color = c }

instance HasProp Disabled (Header ms) where
    type Prop Disabled (Header ms) = Bool
    getProp _ = disabled
    setProp _ d h = h { disabled = d }

instance HasProp Dividing (Header ms) where
    type Prop Dividing (Header ms) = Bool
    getProp _ = dividing
    setProp _ d h = h { dividing = d }

instance HasProp Floated (Header ms) where
    type Prop Floated (Header ms) = Txt
    getProp _ = floated
    setProp _ f h = h { floated = f }

instance HasProp Inverted (Header ms) where
    type Prop Inverted (Header ms) = Bool
    getProp _ = inverted
    setProp _ i h = h { inverted = i }

instance HasProp Size (Header ms) where
    type Prop Size (Header ms) = Txt
    getProp _ = size
    setProp _ s h = h { size = s }

instance HasProp Sub (Header ms) where
    type Prop Sub (Header ms) = Bool
    getProp _ = sub
    setProp _ s h = h { sub = s }

instance HasProp TextAlign (Header ms) where
    type Prop TextAlign (Header ms) = Txt
    getProp _ = textAlign
    setProp _ ta h = h { textAlign = ta }

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

instance HasProp As (Content ms) where
    type Prop As (Content ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f hc = hc { as = f }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs hc = hc { attributes = cs }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs hc = hc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs hc = hc { classes = cs }

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

instance HasProp As (Subheader ms) where
    type Prop As (Subheader ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f hs = hs { as = f }

instance HasProp Attributes (Subheader ms) where
    type Prop Attributes (Subheader ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs hs = hs { attributes = cs }

instance HasProp Children (Subheader ms) where
    type Prop Children (Subheader ms) = [View ms]
    getProp _ = children
    setProp _ cs hs = hs { children = cs }

instance HasProp Classes (Subheader ms) where
    type Prop Classes (Subheader ms) = [Txt]
    getProp _ = classes
    setProp _ cs hs = hs { classes = cs }
