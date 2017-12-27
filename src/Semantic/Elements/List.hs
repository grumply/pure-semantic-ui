{-# LANGUAGE UndecidableInstances #-}
module Semantic.Elements.List (module Semantic.Elements.List, module Export) where

import GHC.Generics as G
import Pure.View hiding (onClick)

import Semantic.Utils

import Semantic.Elements.List.ListContent as Export
import Semantic.Elements.List.ListDescription as Export
import Semantic.Elements.List.ListHeader as Export
import Semantic.Elements.List.ListIcon as Export
import Semantic.Elements.List.ListItem as Export
import Semantic.Elements.List.ListList as Export

import Semantic.Extensions.Animated
import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data List ms = List_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , animated :: Bool
    , bulleted :: Bool
    , celled :: Bool
    , divided :: Bool
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , link :: Bool
    , itemClick :: ListItem ms -> Ef ms IO ()
    , ordered :: Bool
    , relaxed :: Maybe Txt
    , selection :: Bool
    , size :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (List ms) where
    def = (G.to gdef) { as = Div }

pattern List :: VC ms => List ms -> View ms
pattern List l = View l

instance VC ms => Pure List ms where
    render List_ {..} =
        let
            children' =
                mapPures (\li@(ListItem_ {}) -> li { click = click li >> itemClick li }) children

            cs =
                ( "ui"
                : size
                : animated # "animated"
                : bulleted # "bulleted"
                : celled # "celled"
                : divided # "divided"
                : horizontal # "horizontal"
                : inverted # "inverted"
                : link # "link"
                : ordered # "ordered"
                : selection # "selection"
                : may (<>> "relaxed") relaxed
                : floated # (floated <>> "floated")
                : verticalAlign # (verticalAlign <>> "aligned")
                : "list"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children'

instance HasAnimated (List ms) where
    type Anim (List ms) = Bool
    getAnimated = animated
    setAnimated anim l = l { animated = anim }

instance HasAs (List ms) where
    type Constructor (List ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f l = l { as = f }

instance HasAttributes (List ms) where
    type Attribute (List ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs l = l { attributes = cs }

instance HasChildren (List ms) where
    type Child (List ms) = View ms
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasClasses (List ms) where
    getClasses = classes
    setClasses cs l = l { classes = cs }