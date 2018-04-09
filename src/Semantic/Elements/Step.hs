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
import Pure.View hiding (active,completed,disabled,onClick,vertical,widths,Content,Title,Description,Step,Ref)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Active, Active(..)
  , pattern Completed, Completed(..)
  , pattern Disabled, Disabled(..)
  , pattern Ref, Ref(..)
  , pattern Link, Link(..)
  , pattern OnClick, OnClick(..)
  , pattern Ordered, Ordered(..)
  , pattern Attached, Attached(..)
  , pattern Fluid, Fluid(..)
  , pattern Ordered, Ordered(..)
  , pattern Size, Size(..)
  , pattern Stackable, Stackable(..)
  , pattern Unstackable, Unstackable(..)
  , pattern Vertical, Vertical(..)
  , pattern Widths, Widths(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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

instance HasProp As (Step ms) where
    type Prop As (Step ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a s = s { as = a }

instance HasProp Attributes (Step ms) where
    type Prop Attributes (Step ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as s = s { attributes = as }

instance HasProp Children (Step ms) where
    type Prop Children (Step ms) = [View ms]
    getProp _ = children
    setProp _ cs s = s { children = cs }

instance HasProp Classes (Step ms) where
    type Prop Classes (Step ms) = [Txt]
    getProp _ = classes
    setProp _ cs s = s { classes = cs }

instance HasProp Active (Step ms) where
    type Prop Active (Step ms) = Bool
    getProp _ = active
    setProp _ a s = s { active = a }

instance HasProp Completed (Step ms) where
    type Prop Completed (Step ms) = Bool
    getProp _ = completed
    setProp _ c s = s { completed = c }

instance HasProp Disabled (Step ms) where
    type Prop Disabled (Step ms) = Bool
    getProp _ = disabled
    setProp _ d s = s { disabled = d }

instance HasProp Ref (Step ms) where
    type Prop Ref (Step ms) = Feature ms
    getProp _ = ref
    setProp _ r s = s { ref = r }

instance HasProp Link (Step ms) where
    type Prop Link (Step ms) = Bool
    getProp _ = link
    setProp _ l s = s { link = l }

instance HasProp OnClick (Step ms) where
    type Prop OnClick (Step ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc s = s { onClick = oc }

instance HasProp Ordered (Step ms) where
    type Prop Ordered (Step ms) = Bool
    getProp _ = ordered
    setProp _ o s = s { ordered = o }

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

instance HasProp As (Content ms) where
    type Prop As (Content ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sc = sc { as = a }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sc = sc { attributes = as }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs sc = sc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs sc = sc { classes = cs }

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

instance HasProp As (Description ms) where
    type Prop As (Description ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sd = sd { as = a }

instance HasProp Attributes (Description ms) where
    type Prop Attributes (Description ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sd = sd { attributes = as }

instance HasProp Children (Description ms) where
    type Prop Children (Description ms) = [View ms]
    getProp _ = children
    setProp _ cs sd = sd { children = cs }

instance HasProp Classes (Description ms) where
    type Prop Classes (Description ms) = [Txt]
    getProp _ = classes
    setProp _ cs sd = sd { classes = cs }

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
    , widths :: Txt
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

instance HasProp As (Group ms) where
    type Prop As (Group ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sg = sg { as = a }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sg = sg { attributes = as }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs sg = sg { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs sg = sg { classes = cs }

instance HasProp Attached (Group ms) where
    type Prop Attached (Group ms) = Maybe Txt
    getProp _ = attached
    setProp _ a sg = sg { attached = a }

instance HasProp Fluid (Group ms) where
    type Prop Fluid (Group ms) = Bool
    getProp _ = fluid
    setProp _ f sg = sg { fluid = f }

instance HasProp Ordered (Group ms) where
    type Prop Ordered (Group ms) = Bool
    getProp _ = ordered
    setProp _ o sg = sg { ordered = o }

instance HasProp Size (Group ms) where
    type Prop Size (Group ms) = Txt
    getProp _ = size
    setProp _ s sg = sg { size = s }

instance HasProp Stackable (Group ms) where
    type Prop Stackable (Group ms) = Txt
    getProp _ = stackable
    setProp _ s sg = sg { stackable = s }

instance HasProp Unstackable (Group ms) where
    type Prop Unstackable (Group ms) = Bool
    getProp _ = unstackable
    setProp _ u sg = sg { unstackable = u }

instance HasProp Vertical (Group ms) where
    type Prop Vertical (Group ms) = Bool
    getProp _ = vertical
    setProp _ v sg = sg { vertical = v }

instance HasProp Widths (Group ms) where
    type Prop Widths (Group ms) = Txt
    getProp _ = widths
    setProp _ w sg = sg { widths = w }

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

instance HasProp As (Title ms) where
    type Prop As (Title ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a st = st { as = a }

instance HasProp Attributes (Title ms) where
    type Prop Attributes (Title ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as st = st { attributes = as }

instance HasProp Children (Title ms) where
    type Prop Children (Title ms) = [View ms]
    getProp _ = children
    setProp _ cs st = st { children = cs }

instance HasProp Classes (Title ms) where
    type Prop Classes (Title ms) = [Txt]
    getProp _ = classes
    setProp _ cs st = st { classes = cs }

