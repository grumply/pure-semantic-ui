module Semantic.Elements.Step
  ( module Properties
  , module Tools
  , Step(..), pattern Step
  , Content(..), pattern Content
  , Description(..), pattern Description
  , Group(..), pattern Group
  , Title(..), pattern Title
  ) where

import GHC.Generics as G
import Pure.View hiding (active,completed,disabled,onClick,vertical,widths,Content,Title,Description,Step)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActiveProp(..), pattern Active
  , HasCompletedProp(..), pattern Completed
  , HasDisabledProp(..), pattern Disabled
  , HasRefProp(..), pattern Ref
  , HasLinkProp(..), pattern Link
  , HasOnClickProp(..), pattern OnClick
  , HasOrderedProp(..), pattern Ordered
  , HasAttachedProp(..), pattern Attached
  , HasFluidProp(..), pattern Fluid
  , HasOrderedProp(..), pattern Ordered
  , HasSizeProp(..), pattern Size
  , HasStackableProp(..), pattern Stackable
  , HasUnstackableProp(..), pattern Unstackable
  , HasVerticalProp(..), pattern Vertical
  , HasWidthsProp(..), pattern Widths
  )

data Step ms = Step_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , completed :: Bool
    , disabled :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    , ordered :: Bool
    } deriving (Generic)

instance Default (Step ms) where
    def = (G.to gdef) { as = Div }

pattern Step :: Step ms -> View ms
pattern Step s = View s

instance Pure Step ms where
    render Step_ {..} =
        let
            e = onClick ? A $ as

            cs =
                ( active # "active"
                : completed # "completed"
                : disabled # "disabled"
                : link # "link"
                : "step"
                : classes
                )
        in
            e
                ( mergeClasses $ ClassList cs
                : ref
                : onClick # (disabled #! On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasAsProp (Step ms) where
    type AsProp (Step ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a s = s { as = a }

instance HasAttributesProp (Step ms) where
    type Attribute (Step ms) = Feature ms
    getAttributes = attributes
    setAttributes as s = s { attributes = as }

instance HasChildrenProp (Step ms) where
    type Child (Step ms) = View ms
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasClassesProp (Step ms) where
    getClasses = classes
    setClasses cs s = s { classes = cs }

instance HasActiveProp (Step ms) where
    getActive = active
    setActive a s = s { active = a }

instance HasCompletedProp (Step ms) where
    getCompleted = completed
    setCompleted c s = s { completed = c }

instance HasDisabledProp (Step ms) where
    getDisabled = disabled
    setDisabled d s = s { disabled = d }

instance HasRefProp (Step ms) where
    type RefProp (Step ms) = Feature ms
    getRef = ref
    setRef r s = s { ref = r }

instance HasLinkProp (Step ms) where
    getLink = link
    setLink l s = s { link = l }

instance HasOnClickProp (Step ms) where
    type OnClickProp (Step ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc s = s { onClick = oc }

instance HasOrderedProp (Step ms) where
    getOrdered = ordered
    setOrdered o s = s { ordered = o }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content sc = View sc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sc = sc { as = a }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes as sc = sc { attributes = as }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs sc = sc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs sc = sc { classes = cs }

data Description ms = Description_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Description ms) where
    def = (G.to gdef) { as = Div }

pattern Description :: Description ms -> View ms
pattern Description sd = View sd

instance Pure Description ms where
    render Description_ {..} =
        let
            cs =
                ( "description"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Description ms) where
    type AsProp (Description ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sd = sd { as = a }

instance HasAttributesProp (Description ms) where
    type Attribute (Description ms) = Feature ms
    getAttributes = attributes
    setAttributes as sd = sd { attributes = as }

instance HasChildrenProp (Description ms) where
    type Child (Description ms) = View ms
    getChildren = children
    setChildren cs sd = sd { children = cs }

instance HasClassesProp (Description ms) where
    getClasses = classes
    setClasses cs sd = sd { classes = cs }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , fluid :: Bool
    , ordered :: Bool
    , size :: Txt
    , stackable :: Txt
    , unstackable :: Bool
    , vertical :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group sg = View sg

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : size
                : fluid # "fluid"
                : ordered # "ordered"
                : unstackable # "unstackable"
                : vertical # "vertical"
                : may (<>> "attached") attached
                : stackable # (stackable <>> "stackable")
                : widthProp widths def def
                : "steps"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Group ms) where
    type AsProp (Group ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sg = sg { as = a }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes as sg = sg { attributes = as }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs sg = sg { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs sg = sg { classes = cs }

instance HasAttachedProp (Group ms) where
    type AttachedProp (Group ms) = Maybe Txt
    getAttached = attached
    setAttached a sg = sg { attached = a }

instance HasFluidProp (Group ms) where
    getFluid = fluid
    setFluid f sg = sg { fluid = f }

instance HasOrderedProp (Group ms) where
    getOrdered = ordered
    setOrdered o sg = sg { ordered = o }

instance HasSizeProp (Group ms) where
    getSize = size
    setSize s sg = sg { size = s }

instance HasStackableProp (Group ms) where
    getStackable = stackable
    setStackable s sg = sg { stackable = s }

instance HasUnstackableProp (Group ms) where
    getUnstackable = unstackable
    setUnstackable u sg = sg { unstackable = u }

instance HasVerticalProp (Group ms) where
    getVertical = vertical
    setVertical v sg = sg { vertical = v }

instance HasWidthsProp (Group ms) where
    getWidths = widths
    setWidths w sg = sg { widths = w }

data Title ms = Title_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Title ms) where
    def = (G.to gdef) { as = Div }

pattern Title :: Title ms -> View ms
pattern Title st = View st

instance Pure Title ms where
    render Title_ {..} =
        let
            cs =
                ( "title"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Title ms) where
    type AsProp (Title ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a st = st { as = a }

instance HasAttributesProp (Title ms) where
    type Attribute (Title ms) = Feature ms
    getAttributes = attributes
    setAttributes as st = st { attributes = as }

instance HasChildrenProp (Title ms) where
    type Child (Title ms) = View ms
    getChildren = children
    setChildren cs st = st { children = cs }

instance HasClassesProp (Title ms) where
    getClasses = classes
    setClasses cs st = st { classes = cs }

