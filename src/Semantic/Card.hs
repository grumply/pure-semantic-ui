module Semantic.Card
  ( module Properties
  , module Tools
  , Card(..), pattern Card
  , Content(..), pattern Content
  , Description(..), pattern Description
  , Group(..), pattern Group
  , Header(..), pattern Header
  , Meta(..), pattern Meta
  ) where

import GHC.Generics as G hiding (Meta)
import Pure.View hiding (color,onClick,textAlign,Content,Header,Meta,Description,Ref)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Centered, Centered(..)
  , pattern Color, Color(..)
  , pattern Fluid, Fluid(..)
  , pattern Ref, Ref(..)
  , pattern Link, Link(..)
  , pattern OnClick, OnClick(..)
  , pattern Raised, Raised(..)
  , pattern Extra, Extra(..)
  , pattern TextAlign, TextAlign(..)
  , pattern Doubling, Doubling(..)
  , pattern ItemsPerRow, ItemsPerRow(..)
  , pattern Stackable, Stackable(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Card ms = Card_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , color :: Txt
    , fluid :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    , raised :: Bool
    } deriving (Generic)

instance Default (Card ms) where
    def = (G.to gdef) { as = Div }

pattern Card :: Card ms -> View ms
pattern Card a = View a

instance Pure Card ms where
    render Card_ {..} =
        let
            e = onClick ? A $ as
            cs =
                ( "ui"
                : color
                : centered # "centered"
                : fluid # "fluid"
                : link # "link"
                : raised # "raised"
                : "card"
                : classes
                )
        in
            e
                ( mergeClasses $ ClassList cs
                : ref
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasProp As (Card ms) where
    type Prop As (Card ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a c = c { as = a }

instance HasProp Attributes (Card ms) where
    type Prop Attributes (Card ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as c = c { attributes = as }

instance HasProp Children (Card ms) where
    type Prop Children (Card ms) = [View ms]
    getProp _ = children
    setProp _ cs c = c { children = cs }

instance HasProp Classes (Card ms) where
    type Prop Classes (Card ms) = [Txt]
    getProp _ = classes
    setProp _ cs c = c { classes = cs }

instance HasProp Centered (Card ms) where
    type Prop Centered (Card ms) = Bool
    getProp _ = centered
    setProp _ c crd = crd { centered = c }

instance HasProp Color (Card ms) where
    type Prop Color (Card ms) = Txt
    getProp _ = color
    setProp _ c crd = crd { color = c }

instance HasProp Fluid (Card ms) where
    type Prop Fluid (Card ms) = Bool
    getProp _ = fluid
    setProp _ f c = c { fluid = f }

instance HasProp Ref (Card ms) where
    type Prop Ref (Card ms) = Feature ms
    getProp _ = ref
    setProp _ r c = c { ref = r }

instance HasProp Link (Card ms) where
    type Prop Link (Card ms) = Bool
    getProp _ = link
    setProp _ l c = c { link = l }

instance HasProp OnClick (Card ms) where
    type Prop OnClick (Card ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc c = c { onClick = oc }

instance HasProp Raised (Card ms) where
    type Prop Raised (Card ms) = Bool
    getProp _ = raised
    setProp _ r c = c { raised = r }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , extra :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content cc = View cc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( extra # "extra"
                : textAlign
                : "content"
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
    setProp _ a cc = cc { as = a }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as cc = cc { attributes = as }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs cc = cc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs cc = cc { classes = cs }

instance HasProp Extra (Content ms) where
    type Prop Extra (Content ms) = Bool
    getProp _ = extra
    setProp _ e cc = cc { extra = e }

instance HasProp TextAlign (Content ms) where
    type Prop TextAlign (Content ms) = Txt
    getProp _ = textAlign
    setProp _ ta cc = cc { textAlign = ta }

data Description ms = Description_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Description ms) where
    def = (G.to gdef) { as = Div }

pattern Description :: Description ms -> View ms
pattern Description cd = View cd

instance Pure Description ms where
    render Description_ {..} =
        let
            cs =
                ( textAlign
                : "description"
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
    setProp _ a cd = cd { as = a }

instance HasProp Attributes (Description ms) where
    type Prop Attributes (Description ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as cd = cd { attributes = as }

instance HasProp Children (Description ms) where
    type Prop Children (Description ms) = [View ms]
    getProp _ = children
    setProp _ cs cd = cd { children = cs }

instance HasProp Classes (Description ms) where
    type Prop Classes (Description ms) = [Txt]
    getProp _ = classes
    setProp _ cs cd = cd { classes = cs }

instance HasProp TextAlign (Description ms) where
    type Prop TextAlign (Description ms) = Txt
    getProp _ = textAlign
    setProp _ ta cd = cd { textAlign = ta }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , doubling :: Bool
    , itemsPerRow :: Txt
    , stackable :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group cg = View cg

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : doubling # "doubling"
                : stackable # "stackable"
                : textAlign
                : widthProp def width def
                : "cards"
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
    setProp _ a cg = cg { as = a }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as cg = cg { attributes = as }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs cg = cg { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs cg = cg { classes = cs }

instance HasProp Doubling (Group ms) where
    type Prop Doubling (Group ms) = Bool
    getProp _ = doubling
    setProp _ d cg = cg { doubling = d }

instance HasProp ItemsPerRow (Group ms) where
    type Prop ItemsPerRow (Group ms) = Txt
    getProp _ = itemsPerRow
    setProp _ ipr cg = cg { itemsPerRow = ipr }

instance HasProp Stackable (Group ms) where
    type Prop Stackable (Group ms) = Bool
    getProp _ = stackable
    setProp _ s cg = cg { stackable = s }

instance HasProp TextAlign (Group ms) where
    type Prop TextAlign (Group ms) = Txt
    getProp _ = textAlign
    setProp _ ta cc = cc { textAlign = ta }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header ch = View ch

instance Pure Header ms where
    render Header_ {..} =
        let
            cs =
                ( textAlign
                : "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Header ms) where
    type Prop As (Header ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ch = ch { as = a }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ch = ch { attributes = as }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs ch = ch { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs ch = ch { classes = cs }

instance HasProp TextAlign (Header ms) where
    type Prop TextAlign (Header ms) = Txt
    getProp _ = textAlign
    setProp _ ta ch = ch { textAlign = ta }

data Meta ms = Meta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Meta ms) where
    def = (G.to gdef) { as = Div }

pattern Meta :: Meta ms -> View ms
pattern Meta cm = View cm

instance Pure Meta ms where
    render Meta_ {..} =
        let
            cs =
                ( textAlign
                : "meta"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Meta ms) where
    type Prop As (Meta ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a cm = cm { as = a }

instance HasProp Attributes (Meta ms) where
    type Prop Attributes (Meta ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as cm = cm { attributes = as }

instance HasProp Children (Meta ms) where
    type Prop Children (Meta ms) = [View ms]
    getProp _ = children
    setProp _ cs cm = cm { children = cs }

instance HasProp Classes (Meta ms) where
    type Prop Classes (Meta ms) = [Txt]
    getProp _ = classes
    setProp _ cs cm = cm { classes = cs }

instance HasProp TextAlign (Meta ms) where
    type Prop TextAlign (Meta ms) = Txt
    getProp _ = textAlign
    setProp _ ta cm = cm { textAlign = ta }
