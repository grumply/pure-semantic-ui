module Semantic.Elements.Label (module Semantic.Elements.Label, module Export) where

import GHC.Generics as G
import Pure.View as View hiding (active)

import Semantic.Utils

import Semantic.Elements.Label.LabelDetail as Export
import Semantic.Elements.Label.LabelGroup as Export

import Semantic.Elements.Image

import Semantic.Properties.Active
import Semantic.Properties.As
import Semantic.Properties.Attached
import Semantic.Properties.Attributes
import Semantic.Properties.Basic
import Semantic.Properties.Children
import Semantic.Properties.Classes

data Label ms = Label_
    { as :: [Feature ms] -> [View ms] -> View ms
    , active :: Bool
    , attached :: Txt
    , attributes :: [Feature ms]
    , basic :: Bool
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , corner :: Maybe Txt
    , empty :: Bool
    , floating :: Bool
    , horizontal :: Bool
    , click :: Ef ms IO ()
    , pointing :: Maybe Txt
    , ribbon :: Maybe Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (Label ms) where
    def = (G.to gdef) { as = Div }

pattern Label :: Typeable ms => Label ms -> View ms
pattern Label l = View l

instance Typeable ms => Pure Label ms where
    render Label_ {..} =
        let
            pointingClass =
                -- note the careful class ordering
                ($ "pointing") $ flip (maybe (const nil)) pointing $ \case
                    (oneEq "left" "right"  -> Just lr) -> (lr <<>>)
                    (oneEq "above" "below" -> Just ab) -> (<<>> ab)
                    "" -> id
                    _  -> const nil

            hasImage =
                foldPures (\(Image_ {}) -> const True) False children
            
            cs =
                ( "ui"
                : color
                : pointingClass
                : size
                : active # "active"
                : basic # "basic"
                : circular # "circular"
                : empty # "empty"
                : floating # "floating"
                : horizontal # "horizontal"
                : hasImage # "image"
                : tag # "tag"
                : corner # "corner"
                : ribbon # "ribbon"
                : attached # "attached"
                : "label"
                : classes
                )
        in
            as 
                ( ClassList cs
                : click # onClick click
                : attributes
                )
                children

instance HasActiveProp (Label ms) where
    getActive = active
    setActive a l = l { active = a }
            
instance HasAsProp (Label ms) where
    type AsProp (Label ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f l = l { as = f }

instance HasAttachedProp (Label ms) where
    type AttachedProp (Label ms) = Txt
    getAttached = attached
    setAttached attach l = l { attached = attach }

instance HasAttributesProp (Label ms) where
    type Attribute (Label ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs l = l { attributes = cs }

instance HasBasicProp (Label ms) where
    getBasic = basic
    setBasic b l = l { basic = b }

instance HasChildrenProp (Label ms) where
    type Child (Label ms) = View ms
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasClassesProp (Label ms) where
    getClasses = classes
    setClasses cs l = l { classes = cs }