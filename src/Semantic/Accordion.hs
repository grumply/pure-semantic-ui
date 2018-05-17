module Semantic.Accordion
  ( module Properties
  , module Tools
  , Accordion(..), pattern Accordion
  , Subaccordion(..), pattern Subaccordion
  , Content(..), pattern Content
  , Title(..), pattern Title
  ) where

import GHC.Generics as G (Generic,to)
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt hiding (index)
import Pure.Data.HTML hiding (Title)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Fluid, Fluid(..)
  , pattern Inverted, Inverted(..)
  , pattern Styled, Styled(..)
  , pattern Active, Active(..)
  , pattern Index, Index(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Accordion = Accordion_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , fluid :: Bool
    , inverted :: Bool
    , styled :: Bool
    } deriving (Generic)

instance Default Accordion where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Accordion :: Accordion -> Accordion
pattern Accordion a = a

instance Pure Accordion where
    view Accordion_ {..} =
        let
            cs =
                [ "ui"
                , "accordion"
                , fluid # "fluid"
                , inverted # "inverted"
                , styled # "styled"
                ]
        in
            as (features & AddClasses cs) children

instance HasProp As Accordion where
    type Prop As Accordion = Features -> [View] -> View
    getProp _ = as
    setProp _ a acc = acc { as = a }

instance HasFeatures Accordion where
    getFeatures = features
    setFeatures as a = a { features = as }

instance HasChildren Accordion where
    getChildren = children
    setChildren cs a = a { children = cs }

instance HasProp Fluid Accordion where
    type Prop Fluid Accordion = Bool
    getProp _ = fluid
    setProp _ f a = a { fluid = f }

instance HasProp Inverted Accordion where
    type Prop Inverted Accordion = Bool
    getProp _ = inverted
    setProp _ i a = a { inverted = i }

instance HasProp Styled Accordion where
    type Prop Styled Accordion = Bool
    getProp _ = styled
    setProp _ s a = a { styled = s }

data Subaccordion = Subaccordion_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Subaccordion where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Subaccordion :: Subaccordion -> Subaccordion
pattern Subaccordion a = a

instance Pure Subaccordion where
    view Subaccordion_ {..} = as (features & Class "accordion") children

instance HasProp As Subaccordion where
    type Prop As Subaccordion = Features -> [View] -> View
    getProp _ = as
    setProp _ a acc = acc { as = a }

instance HasFeatures Subaccordion where
    getFeatures = features
    setFeatures fs a = a { features = fs }

instance HasChildren Subaccordion where
    getChildren = children
    setChildren cs a = a { children = cs }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content ac = ac

instance Pure Content where
    view Content_ {..} =
        let
            cs =
                [ "content"
                , active # "active"
                ]
        in
            as (features & AddClasses cs) children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ a ac = ac { as = a }

instance HasFeatures Content where
    getFeatures = features
    setFeatures fs ac = ac { features = fs }

instance HasChildren Content where
    getChildren = children
    setChildren cs ac = ac { children = cs }

instance HasProp Active Content where
    type Prop Active Content = Bool
    getProp _ = active
    setProp _ a ac = ac { active = a }

data Title = Title_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , index :: Int
    } deriving (Generic)

instance Default Title where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Title :: Title -> Title
pattern Title at = at

instance Pure Title where
    view Title_ {..} =
        let
            cs =
                [ active # "active"
                , "title"
                ]
        in
            as (features & AddClasses cs) children

instance HasProp As Title where
    type Prop As Title = Features -> [View] -> View
    getProp _ = as
    setProp _ a at = at { as = a }

instance HasFeatures Title where
    getFeatures = features
    setFeatures fs at = at { features = fs }

instance HasChildren Title where
    getChildren = children
    setChildren cs at = at { children = cs }

instance HasProp Active Title where
    type Prop Active Title = Bool
    getProp _ = active
    setProp _ a at = at { active = a }

instance HasProp Index Title where
    type Prop Index Title = Int
    getProp _ = index
    setProp _ i at = at { index = i }
