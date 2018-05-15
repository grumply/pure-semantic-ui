module Semantic.Accordion
  ( module Properties
  , module Tools
  , Accordion(..), pattern Accordion
  , Subaccordion(..), pattern Subaccordion
  , Content(..), pattern Content
  , Title(..), pattern Title
  ) where

import GHC.Generics as G
import Pure.View hiding (styled,onClick,active,Content,Title)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Fluid, Fluid(..)
  , pattern Inverted, Inverted(..)
  , pattern Styled, Styled(..)
  , pattern Active, Active(..)
  , pattern Index, Index(..)
  , pattern OnClick, OnClick(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Accordion ms = Accordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fluid :: Bool
    , inverted :: Bool
    , styled :: Bool
    } deriving (Generic)

instance Default (Accordion ms) where
    def = (G.to gdef) { as = Div }

pattern Accordion :: Accordion ms -> View ms
pattern Accordion a = View a

instance Pure Accordion ms where
    render Accordion_ {..} =
        let
            cs =
                ( "ui"
                : "accordion"
                : fluid # "fluid"
                : inverted # "inverted"
                : styled # "styled"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Accordion ms) where
    type Prop As (Accordion ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a acc = acc { as = a }

instance HasProp Attributes (Accordion ms) where
    type Prop Attributes (Accordion ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as a = a { attributes = as }

instance HasProp Children (Accordion ms) where
    type Prop Children (Accordion ms) = [View ms]
    getProp _ = children
    setProp _ cs a = a { children = cs }

instance HasProp Classes (Accordion ms) where
    type Prop Classes (Accordion ms) = [Txt]
    getProp _ = classes
    setProp _ cs a = a { classes = cs }

instance HasProp Fluid (Accordion ms) where
    type Prop Fluid (Accordion ms) = Bool
    getProp _ = fluid
    setProp _ f a = a { fluid = f }

instance HasProp Inverted (Accordion ms) where
    type Prop Inverted (Accordion ms) = Bool
    getProp _ = inverted
    setProp _ i a = a { inverted = i }

instance HasProp Styled (Accordion ms) where
    type Prop Styled (Accordion ms) = Bool
    getProp _ = styled
    setProp _ s a = a { styled = s }

data Subaccordion ms = Subaccordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Subaccordion ms) where
    def = (G.to gdef) { as = Div }

pattern Subaccordion :: Subaccordion ms -> View ms
pattern Subaccordion a = View a

instance Pure Subaccordion ms where
    render Subaccordion_ {..} =
        let
            cs =
                ( "accordion"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Subaccordion ms) where
    type Prop As (Subaccordion ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a acc = acc { as = a }

instance HasProp Attributes (Subaccordion ms) where
    type Prop Attributes (Subaccordion ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as a = a { attributes = as }

instance HasProp Children (Subaccordion ms) where
    type Prop Children (Subaccordion ms) = [View ms]
    getProp _ = children
    setProp _ cs a = a { children = cs }

instance HasProp Classes (Subaccordion ms) where
    type Prop Classes (Subaccordion ms) = [Txt]
    getProp _ = classes
    setProp _ cs a = a { classes = cs }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content ac = View ac

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( "content"
                : active # "active"
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
    setProp _ a ac = ac { as = a }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ac = ac { attributes = as }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs ac = ac { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs ac = ac { classes = cs }

instance HasProp Active (Content ms) where
    type Prop Active (Content ms) = Bool
    getProp _ = active
    setProp _ a ac = ac { active = a }

data Title ms = Title_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , index :: Int
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (Title ms) where
    def = (G.to gdef) { as = Div }

pattern Title :: Title ms -> View ms
pattern Title at = View at

instance Pure Title ms where
    render Title_ {..} =
        let
            cs =
                ( active # "active"
                : "title"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : onClick # (On "click" def $ \_ -> return (Just onClick))
                : attributes
                )
                children

instance HasProp As (Title ms) where
    type Prop As (Title ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a at = at { as = a }

instance HasProp Attributes (Title ms) where
    type Prop Attributes (Title ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as at = at { attributes = as }

instance HasProp Children (Title ms) where
    type Prop Children (Title ms) = [View ms]
    getProp _ = children
    setProp _ cs at = at { children = cs }

instance HasProp Classes (Title ms) where
    type Prop Classes (Title ms) = [Txt]
    getProp _ = classes
    setProp _ cs at = at { classes = cs }

instance HasProp Active (Title ms) where
    type Prop Active (Title ms) = Bool
    getProp _ = active
    setProp _ a at = at { active = a }

instance HasProp Index (Title ms) where
    type Prop Index (Title ms) = Int
    getProp _ = index
    setProp _ i at = at { index = i }

instance HasProp OnClick (Title ms) where
    type Prop OnClick (Title ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc at = at { onClick = oc }
