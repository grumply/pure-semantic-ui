module Semantic.Elements.Header (module Semantic.Elements.Header, module Export) where

import GHC.Generics as G
import Pure.View hiding (block,Header)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon
import Semantic.Elements.Image

import Semantic.Elements.Header.HeaderContent as Export
import Semantic.Elements.Header.HeaderSubheader as Export

import Semantic.Extensions.As
import Semantic.Extensions.Attached
import Semantic.Extensions.Attributes
import Semantic.Extensions.Block
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

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

pattern Header :: Typeable ms => Header ms -> View ms
pattern Header h = View h

instance Typeable ms => Pure Header ms where
    render Header_ {..} =
        let
            icon = foldr (\(View Icon{}) -> const True) False children
            image = foldr (\(View Image{}) -> const True) False children

            cs =
                ( "ui"
                : color
                : size
                : block # "block"
                : disabled # "disabled"
                : dividing # "dividing"
                : floated # floated <<>> "floated"
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
            as ( ClassList cs
               : attributes
               )
               children

instance HasAs (Header ms) where
    type Constructor (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f h = h { as = f }

instance HasAttached (Header ms) where
    type Attach (Header ms) = Maybe Txt
    getAttached = attached
    setAttached attach h = h { attached = attach }

instance HasAttributes (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs h = h { attributes = cs }

instance HasBlock (Header ms) where
    getBlock = block
    setBlock b h = h { block = b }

instance HasChildren (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs h = h { children = cs }

instance HasClasses (Header ms) where
    getClasses = classes
    setClasses cs h = h { classes = cs }