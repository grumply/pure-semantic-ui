{-# LANGUAGE UndecidableInstances #-}
module Semantic.Elements.KeyedList (module Semantic.Elements.KeyedList, module Export) where

import GHC.Generics as G
import Pure.View hiding (horizontal,onClick,verticalAlign)

import Semantic.Utils

import Semantic.Elements.List.ListContent as Export
import Semantic.Elements.List.ListDescription as Export
import Semantic.Elements.List.ListHeader as Export
import Semantic.Elements.List.ListIcon as Export
import Semantic.Elements.List.ListItem as Export
import Semantic.Elements.List.ListList as Export

import Semantic.Properties.Animated
import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Bulleted
import Semantic.Properties.Celled
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Divided
import Semantic.Properties.Floated
import Semantic.Properties.Horizontal
import Semantic.Properties.Inverted
import Semantic.Properties.Link
import Semantic.Properties.OnClick
import Semantic.Properties.Ordered
import Semantic.Properties.Relaxed
import Semantic.Properties.Selection
import Semantic.Properties.Size
import Semantic.Properties.VerticalAlign

data KeyedList ms = KeyedList_
    { as :: [Feature ms] -> [(Int,View ms)] -> View ms
    , attributes :: [Feature ms]
    , children :: [(Int,View ms)]
    , classes :: [Txt]
    , animated :: Bool
    , bulleted :: Bool
    , celled :: Bool
    , divided :: Bool
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , link :: Bool
    , onItemClick :: (Int,ListItem ms) -> Ef ms IO ()
    , ordered :: Bool
    , relaxed :: Maybe Txt
    , selection :: Bool
    , size :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (KeyedList ms) where
    def = (G.to gdef) { as = list Div }

pattern KeyedList :: VC ms => KeyedList ms -> View ms
pattern KeyedList l = View l

instance VC ms => Pure KeyedList ms where
    render KeyedList_ {..} =
        let
            children' =
                flip map children $ \(n,c) ->
                    case c of
                        View li@ListItem_ {} -> (n,View li { onClick = onClick li >> onItemClick (n,li) }) 
                        _                    -> (n,c)

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

instance HasAnimatedProp (KeyedList ms) where
    type AnimatedProp (KeyedList ms) = Bool
    getAnimated = animated
    setAnimated anim l = l { animated = anim }

instance HasAsProp (KeyedList ms) where
    type AsProp (KeyedList ms) = [Feature ms] -> [(Int,View ms)] -> View ms
    getAs = as
    setAs f l = l { as = f }

instance HasAttributesProp (KeyedList ms) where
    type Attribute (KeyedList ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs l = l { attributes = cs }

instance HasBulletedProp (KeyedList ms) where
    getBulleted = bulleted
    setBulleted b l = l { bulleted = b }

instance HasCelledProp (KeyedList ms) where
    getCelled = celled
    setCelled c l = l { celled = c }

instance HasChildrenProp (KeyedList ms) where
    type Child (KeyedList ms) = (Int,View ms)
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasClassesProp (KeyedList ms) where
    getClasses = classes
    setClasses cs l = l { classes = cs }

instance HasDividedProp (KeyedList ms) where
    getDivided = divided
    setDivided d l = l { divided = d }

instance HasOnClickProp (KeyedList ms) where
    type OnClickProp (KeyedList ms) = (Int,ListItem ms) -> Ef ms IO ()
    getOnClick = onItemClick
    setOnClick oc l = l { onItemClick = oc }

instance HasFloatedProp (KeyedList ms) where
    getFloated = floated
    setFloated f l = l { floated = f }

instance HasHorizontalProp (KeyedList ms) where
    getHorizontal = horizontal
    setHorizontal h l = l { horizontal = h }

instance HasInvertedProp (KeyedList ms) where
    getInverted = inverted
    setInverted i l = l { inverted = i }

instance HasLinkProp (KeyedList ms) where
    getLink = link
    setLink lnk l = l { link = lnk }

instance HasOrderedProp (KeyedList ms) where
    getOrdered = ordered
    setOrdered o l = l { ordered = o }

instance HasRelaxedProp (KeyedList ms) where
    getRelaxed = relaxed
    setRelaxed r l = l { relaxed = r }

instance HasSelectionProp (KeyedList ms) where
    getSelection = selection
    setSelection s l = l { selection = s }

instance HasSizeProp (KeyedList ms) where
    getSize = size
    setSize s l = l { size = s }

instance HasVerticalAlignProp (KeyedList ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va l = l { verticalAlign = va }