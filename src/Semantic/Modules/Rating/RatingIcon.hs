{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Rating.RatingIcon where

import GHC.Generics as G
import Pure.View hiding (active,onClick,onKeyUp,Selected)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Classes
import Semantic.Properties.AriaChecked
import Semantic.Properties.AriaPosinset
import Semantic.Properties.AriaSetsize
import Semantic.Properties.Active
import Semantic.Properties.Index
import Semantic.Properties.OnClick
import Semantic.Properties.OnKeyUp
import Semantic.Properties.OnMouseEnter
import Semantic.Properties.Selected

data RatingIcon ms = RatingIcon_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , active :: Bool
    , ariaChecked :: Bool
    , ariaPosinset :: Int
    , ariaSetsize :: Int
    , index :: Int
    , onClick :: Int -> Ef ms IO ()
    , onKeyUp :: Int -> Evt -> Ef ms IO ()
    , onMouseEnter :: Int -> Ef ms IO ()
    , selected :: Bool
    } deriving (Generic)

instance Default (RatingIcon ms) where
    def = (G.to gdef) { as = I }

pattern RatingIcon :: VC ms => RatingIcon ms -> View ms
pattern RatingIcon ri = View ri

instance VC ms => Pure RatingIcon ms where
    render RatingIcon_ {..} =
        let
            handleClick _ = 
                return $ Just (onClick index)

            handleKeyUp e@Enter = do
                prevDef e
                return $ Just (onKeyUp index e >> onClick index)
            handleKeyUp e@Space = do
                prevDef e
                return $ Just (onKeyUp index e >> onClick index)
            handleKeyUp e = return $ Just $ onKeyUp index e

            handleMouseEnter _ = 
                return $ Just (onMouseEnter index)

            cs =
                ( active # "active"
                : selected # "selected"
                : "icon"
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : Attribute "aria-checked" (ariaChecked ? "true" $ "false")
                : Attribute "aria-posinset" (toTxt ariaPosinset)
                : Attribute "aria-setsize" (toTxt ariaSetsize)
                : On "click" def handleClick
                : On "keyup" def handleKeyUp
                : On "mouseenter" def handleMouseEnter
                : Tabindex 0
                : Role "radio"
                : attributes
                )
                []
                

instance HasAsProp (RatingIcon ms) where
    type AsProp (RatingIcon ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ri = ri { as = a }

instance HasAttributesProp (RatingIcon ms) where
    type Attribute (RatingIcon ms) = Feature ms
    getAttributes = attributes
    setAttributes as ri = ri { attributes = as }

instance HasClassesProp (RatingIcon ms) where
    getClasses = classes
    setClasses cs ri = ri { classes = cs }

instance HasActiveProp (RatingIcon ms) where
    getActive = active
    setActive a ri = ri { active = a }

instance HasAriaCheckedProp (RatingIcon ms) where
    getAriaChecked = ariaChecked
    setAriaChecked ac ri = ri { ariaChecked = ac }

instance HasAriaPosinsetProp (RatingIcon ms) where
    getAriaPosinset = ariaPosinset
    setAriaPosinset ap ri = ri { ariaPosinset = ap }

instance HasAriaSetsizeProp (RatingIcon ms) where
    getAriaSetsize = ariaSetsize
    setAriaSetsize as ri = ri { ariaSetsize = as }

instance HasIndexProp (RatingIcon ms) where
    getIndex = index
    setIndex i ri = ri { index = i }

instance HasOnClickProp (RatingIcon ms) where
    type OnClickProp (RatingIcon ms) = Int -> Ef ms IO ()
    getOnClick = onClick
    setOnClick oc ri = ri { onClick = oc }

instance HasOnKeyUpProp (RatingIcon ms) where
    type OnKeyUpProp (RatingIcon ms) = Int -> Evt -> Ef ms IO ()
    getOnKeyUp = onKeyUp
    setOnKeyUp oku ri = ri { onKeyUp = oku }

instance HasOnMouseEnterProp (RatingIcon ms) where
    type OnMouseEnterProp (RatingIcon ms) = Int -> Ef ms IO ()
    getOnMouseEnter = onMouseEnter
    setOnMouseEnter ome ri = ri { onMouseEnter = ome }

instance HasSelectedProp (RatingIcon ms) where
    getSelected = selected
    setSelected s ri = ri { selected = s }