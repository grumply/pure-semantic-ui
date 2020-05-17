module Semantic.Step
  ( module Properties
  , module Tools
  , Step(..), pattern Step
  , Content(..), pattern Content
  , Description(..), pattern Semantic.Step.Description
  , Group(..), pattern Group
  , Title(..), pattern Semantic.Step.Title
  ) where

import Pure hiding (Content_,Content,Step,vertical,size,link,disabled,active,(#))

import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Active, Active(..)
  , pattern Completed, Completed(..)
  , pattern Disabled, Disabled(..)
  , pattern Ref, Ref(..)
  , pattern Link, Link(..)
  , pattern Ordered, Ordered(..)
  , pattern Attached, Attached(..)
  , pattern Fluid, Fluid(..)
  , pattern Ordered, Ordered(..)
  , pattern Size, Size(..)
  , pattern Stackable, Stackable(..)
  , pattern Unstackable, Unstackable(..)
  , pattern Vertical, Vertical(..)
  , pattern Widths, Widths(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Data.Function as Tools ((&))

data Step = Step_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , completed :: Bool
    , disabled :: Bool
    , link :: Bool
    , ordered :: Bool
    } deriving (Generic)

instance Default Step where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Step :: Step -> Step
pattern Step s = s

instance Pure Step where
    view Step_ {..} =
        let
            cs =
                [ active # "active"
                , completed # "completed"
                , disabled # "disabled"
                , link # "link"
                , "step"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Step where
    type Prop As Step = Features -> [View] -> View
    getProp _ = as
    setProp _ a s = s { as = a }

instance HasFeatures Step where
    getFeatures = features
    setFeatures as s = s { features = as }

instance HasChildren Step where
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasProp Active Step where
    type Prop Active Step = Bool
    getProp _ = active
    setProp _ a s = s { active = a }

instance HasProp Completed Step where
    type Prop Completed Step = Bool
    getProp _ = completed
    setProp _ c s = s { completed = c }

instance HasProp Disabled Step where
    type Prop Disabled Step = Bool
    getProp _ = disabled
    setProp _ d s = s { disabled = d }

instance HasProp Link Step where
    type Prop Link Step = Bool
    getProp _ = link
    setProp _ l s = s { link = l }

instance HasProp Ordered Step where
    type Prop Ordered Step = Bool
    getProp _ = ordered
    setProp _ o s = s { ordered = o }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content sc = sc

instance Pure Content where
    view Content_ {..} = as (features & Class "content") children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ a sc = sc { as = a }

instance HasFeatures Content where
    getFeatures = features
    setFeatures as sc = sc { features = as }

instance HasChildren Content where
    getChildren = children
    setChildren cs sc = sc { children = cs }

data Description = Description_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Description where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Description :: Description -> Description
pattern Description sd = sd

instance Pure Description where
    view Description_ {..} = as (features & Class "description") children

instance HasProp As Description where
    type Prop As Description = Features -> [View] -> View
    getProp _ = as
    setProp _ a sd = sd { as = a }

instance HasFeatures Description where
    getFeatures = features
    setFeatures as sd = sd { features = as }

instance HasChildren Description where
    getChildren = children
    setChildren cs sd = sd { children = cs }

data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , attached :: Maybe Txt
    , fluid :: Bool
    , ordered :: Bool
    , size :: Txt
    , stackable :: Txt
    , unstackable :: Bool
    , vertical :: Bool
    , widths :: Txt
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group sg = sg

instance Pure Group where
    view Group_ {..} =
        let
            cs =
                [ "ui"
                , size
                , fluid # "fluid"
                , ordered # "ordered"
                , unstackable # "unstackable"
                , vertical # "vertical"
                , maybe "" (<>> "attached") attached
                , (stackable /= mempty) # (stackable <>> "stackable")
                , widthProp widths def def
                , "steps"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ a sg = sg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as sg = sg { features = as }

instance HasChildren Group where
    getChildren = children
    setChildren cs sg = sg { children = cs }

instance HasProp Attached Group where
    type Prop Attached Group = Maybe Txt
    getProp _ = attached
    setProp _ a sg = sg { attached = a }

instance HasProp Fluid Group where
    type Prop Fluid Group = Bool
    getProp _ = fluid
    setProp _ f sg = sg { fluid = f }

instance HasProp Ordered Group where
    type Prop Ordered Group = Bool
    getProp _ = ordered
    setProp _ o sg = sg { ordered = o }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ s sg = sg { size = s }

instance HasProp Stackable Group where
    type Prop Stackable Group = Txt
    getProp _ = stackable
    setProp _ s sg = sg { stackable = s }

instance HasProp Unstackable Group where
    type Prop Unstackable Group = Bool
    getProp _ = unstackable
    setProp _ u sg = sg { unstackable = u }

instance HasProp Vertical Group where
    type Prop Vertical Group = Bool
    getProp _ = vertical
    setProp _ v sg = sg { vertical = v }

instance HasProp Widths Group where
    type Prop Widths Group = Txt
    getProp _ = widths
    setProp _ w sg = sg { widths = w }

data Title = Title_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Title where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Title :: Title -> Title
pattern Title st = st

instance Pure Title where
    view Title_ {..} = as (features & Class "title") children

instance HasProp As Title where
    type Prop As Title = Features -> [View] -> View
    getProp _ = as
    setProp _ a st = st { as = a }

instance HasFeatures Title where
    getFeatures = features
    setFeatures as st = st { features = as }

instance HasChildren Title where
    getChildren = children
    setChildren cs st = st { children = cs }

