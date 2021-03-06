module Semantic.Card
  ( module Properties
  , module Tools
  , Card(..), pattern Card
  , Content(..), pattern Content
  , Description(..), pattern Semantic.Card.Description
  , Group(..), pattern Group
  , Header(..), pattern Semantic.Card.Header
  , Meta(..), pattern Semantic.Card.Meta
  ) where

import Pure hiding (Content,Content_,color,link,(#))

import GHC.Generics as G hiding (Meta)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Centered, Centered(..)
  , pattern Color, Color(..)
  , pattern Fluid, Fluid(..)
  , pattern Ref, Ref(..)
  , pattern Link, Link(..)
  , pattern Raised, Raised(..)
  , pattern Extra, Extra(..)
  , pattern TextAlign, TextAlign(..)
  , pattern Doubling, Doubling(..)
  , pattern ItemsPerRow, ItemsPerRow(..)
  , pattern Stackable, Stackable(..)
  )

import Data.Function as Tools ((&))

data Card = Card_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , centered :: Bool
    , color :: Txt
    , fluid :: Bool
    , link :: Bool
    , raised :: Bool
    } deriving (Generic)

instance Default Card where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Card :: Card -> Card
pattern Card a = a

instance Pure Card where
    view Card_ {..} =
        let
            cs =
                [ "ui"
                , color
                , centered # "centered"
                , fluid # "fluid"
                , link # "link"
                , raised # "raised"
                , "card"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Card where
    type Prop As Card = Features -> [View] -> View
    getProp _ = as
    setProp _ a c = c { as = a }

instance HasFeatures Card where
    getFeatures = features
    setFeatures as c = c { features = as }

instance HasChildren Card where
    getChildren = children
    setChildren cs c = c { children = cs }

instance HasProp Centered Card where
    type Prop Centered Card = Bool
    getProp _ = centered
    setProp _ c crd = crd { centered = c }

instance HasProp Color Card where
    type Prop Color Card = Txt
    getProp _ = color
    setProp _ c crd = crd { color = c }

instance HasProp Fluid Card where
    type Prop Fluid Card = Bool
    getProp _ = fluid
    setProp _ f c = c { fluid = f }

instance HasProp Link Card where
    type Prop Link Card = Bool
    getProp _ = link
    setProp _ l c = c { link = l }

instance HasProp Raised Card where
    type Prop Raised Card = Bool
    getProp _ = raised
    setProp _ r c = c { raised = r }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , extra :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content cc = cc

instance Pure Content where
    view Content_ {..} =
        let
            cs =
                [ extra # "extra"
                , textAlign
                , "content"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ a cc = cc { as = a }

instance HasFeatures Content where
    getFeatures = features
    setFeatures as cc = cc { features = as }

instance HasChildren Content where
    getChildren = children
    setChildren cs cc = cc { children = cs }

instance HasProp Extra Content where
    type Prop Extra Content = Bool
    getProp _ = extra
    setProp _ e cc = cc { extra = e }

instance HasProp TextAlign Content where
    type Prop TextAlign Content = Txt
    getProp _ = textAlign
    setProp _ ta cc = cc { textAlign = ta }

data Description = Description_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , textAlign :: Txt
    } deriving (Generic)

instance Default Description where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Description :: Description -> Description
pattern Description cd = cd

instance Pure Description where
    view Description_ {..} = as (features & Classes [ textAlign, "description" ]) children

instance HasProp As Description where
    type Prop As Description = Features -> [View] -> View
    getProp _ = as
    setProp _ a cd = cd { as = a }

instance HasFeatures Description where
    getFeatures = features
    setFeatures as cd = cd { features = as }

instance HasChildren Description where
    getChildren = children
    setChildren cs cd = cd { children = cs }

instance HasProp TextAlign Description where
    type Prop TextAlign Description = Txt
    getProp _ = textAlign
    setProp _ ta cd = cd { textAlign = ta }

data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , doubling :: Bool
    , itemsPerRow :: Txt
    , stackable :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group cg = cg

instance Pure Group where
    view Group_ {..} =
        let
            cs =
                [ "ui"
                , doubling # "doubling"
                , stackable # "stackable"
                , textAlign
                , widthProp def "width" def
                , "cards"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ a cg = cg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as cg = cg { features = as }

instance HasChildren Group where
    getChildren = children
    setChildren cs cg = cg { children = cs }

instance HasProp Doubling Group where
    type Prop Doubling Group = Bool
    getProp _ = doubling
    setProp _ d cg = cg { doubling = d }

instance HasProp ItemsPerRow Group where
    type Prop ItemsPerRow Group = Txt
    getProp _ = itemsPerRow
    setProp _ ipr cg = cg { itemsPerRow = ipr }

instance HasProp Stackable Group where
    type Prop Stackable Group = Bool
    getProp _ = stackable
    setProp _ s cg = cg { stackable = s }

instance HasProp TextAlign Group where
    type Prop TextAlign Group = Txt
    getProp _ = textAlign
    setProp _ ta cc = cc { textAlign = ta }

data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , textAlign :: Txt
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header ch = ch

instance Pure Header where
    view Header_ {..} = as (features & Classes [ textAlign, "header" ]) children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ a ch = ch { as = a }

instance HasFeatures Header where
    getFeatures = features
    setFeatures as ch = ch { features = as }

instance HasChildren Header where
    getChildren = children
    setChildren cs ch = ch { children = cs }

instance HasProp TextAlign Header where
    type Prop TextAlign Header = Txt
    getProp _ = textAlign
    setProp _ ta ch = ch { textAlign = ta }

data Meta = Meta_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , textAlign :: Txt
    } deriving (Generic)

instance Default Meta where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Meta :: Meta -> Meta
pattern Meta cm = cm

instance Pure Meta where
    view Meta_ {..} = as (features & Classes [ textAlign, "meta" ]) children

instance HasProp As Meta where
    type Prop As Meta = Features -> [View] -> View
    getProp _ = as
    setProp _ a cm = cm { as = a }

instance HasFeatures Meta where
    getFeatures = features
    setFeatures as cm = cm { features = as }

instance HasChildren Meta where
    getChildren = children
    setChildren cs cm = cm { children = cs }

instance HasProp TextAlign Meta where
    type Prop TextAlign Meta = Txt
    getProp _ = textAlign
    setProp _ ta cm = cm { textAlign = ta }
