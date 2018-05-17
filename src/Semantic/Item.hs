module Semantic.Item
  ( module Properties
  , module Tools
  , Item(..), pattern Item
  , Content(..), pattern Content
  , Description(..), pattern Description
  , Extra(..), pattern Extra
  , Group(..), pattern Group
  , Header(..), pattern Header
  , Image(..), pattern Image
  , Meta(..), pattern Meta
  ) where

import GHC.Generics as G hiding (Meta)
import Pure.View hiding (verticalAlign,disabled,inline,hidden,Content,Description,Header,Meta)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Divided, Divided(..)
  , pattern Link, Link(..)
  , pattern Relaxed, Relaxed(..)
  , pattern Unstackable, Unstackable(..)
  , pattern Avatar, Avatar(..)
  , pattern Bordered, Bordered(..)
  , pattern Centered, Centered(..)
  , pattern Circular, Circular(..)
  , pattern Disabled, Disabled(..)
  , pattern Floated, Floated(..)
  , pattern Fluid, Fluid(..)
  , pattern Hidden, Hidden(..)
  , pattern Inline, Inline(..)
  , pattern Rounded, Rounded(..)
  , pattern Size, Size(..)
  , pattern Spaced, Spaced(..)
  , pattern UI, UI(..)
  , pattern Wrapped, Wrapped(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Item = Item_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Item where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Item :: Item -> Item
pattern Item i = i

instance Pure Item where
    render Item_ {..} =
        let
            cs =
                ( "item"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Item where
    type Prop As Item = Features -> [View] -> View
    getProp _ = as
    setProp _ a i = i { as = a }

instance HasFeatures Item where
    getFeatures = features
    setFeatures as i = i { features = as }

instance HasChildren Item where
    getChildren = children
    setChildren cs i = i { children = cs }


data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content ic = ic

instance Pure Content where
    render Content_ {..} =
        let
            cs =
                ( verticalAlign
                : "content"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ a ic = ic { as = a }

instance HasFeatures Content where
    getFeatures = features
    setFeatures as ic = ic { features = as }

instance HasChildren Content where
    getChildren = children
    setChildren cs ic = ic { children = cs }


instance HasProp VerticalAlign Content where
    type Prop VerticalAlign Content = Txt
    getProp _ = verticalAlign
    setProp _ va ic = ic { verticalAlign = va }

data Description = Description_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Description where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Description :: Description -> Description
pattern Description id = id

instance Pure Description where
    render Description_ {..} =
        let
            cs =
                ( "description"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Description where
    type Prop As Description = Features -> [View] -> View
    getProp _ = as
    setProp _ a id = id { as = a }

instance HasFeatures Description where
    getFeatures = features
    setFeatures as id = id { features = as }

instance HasChildren Description where
    getChildren = children
    setChildren cs id = id { children = cs }


data Extra = Extra_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Extra where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Extra :: Extra -> Extra
pattern Extra ie = ie

instance Pure Extra where
    render Extra_ {..} =
        let
            cs =
                ( "extra"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Extra where
    type Prop As Extra = Features -> [View] -> View
    getProp _ = as
    setProp _ a ie = ie { as = a }

instance HasFeatures Extra where
    getFeatures = features
    setFeatures as ie = ie { features = as }

instance HasChildren Extra where
    getChildren = children
    setChildren cs ie = ie { children = cs }


data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , divided :: Bool
    , link :: Bool
    , relaxed :: Maybe Txt
    , unstackable :: Bool
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group ig = ig

instance Pure Group where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : divided # "divided"
                : link # "link"
                : unstackable # "unstackable"
                : may (<>> "relaxed") relaxed
                : "items"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ a ig = ig { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as ig = ig { features = as }

instance HasChildren Group where
    getChildren = children
    setChildren cs ig = ig { children = cs }


instance HasProp Divided Group where
    type Prop Divided Group = Bool
    getProp _ = divided
    setProp _ d ig = ig { divided = d }

instance HasProp Link Group where
    type Prop Link Group = Bool
    getProp _ = link
    setProp _ l ig = ig { link = l }

instance HasProp Relaxed Group where
    type Prop Relaxed Group = Maybe Txt
    getProp _ = relaxed
    setProp _ r ig = ig { relaxed = r }

instance HasProp Unstackable Group where
    type Prop Unstackable Group = Bool
    getProp _ = unstackable
    setProp _ u ig = ig { unstackable = u }

data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header ih = ih

instance Pure Header where
    render Header_ {..} =
        let
            cs =
                ( "header"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ a ih = ih { as = a }

instance HasFeatures Header where
    getFeatures = features
    setFeatures as ih = ih { features = as }

instance HasChildren Header where
    getChildren = children
    setChildren cs ih = ih { children = cs }


data Image = Image_
    { as :: Features -> [View] -> View
    , features :: Features
    , avatar :: Bool
    , bordered :: Bool
    , centered :: Bool
    , children :: [View]
    , circular :: Bool
    , disabled :: Bool
    , floated :: Txt
    , fluid :: Bool
    , hidden :: Bool
    , inline :: Bool
    , rounded :: Bool
    , size :: Txt
    , spaced :: Maybe Txt
    , verticalAlign :: Txt
    , wrapped :: Bool
    } deriving (Generic)

instance Default Image where
    def = (G.to gdef) { as = \fs cs -> Img & Features fs & Children cs }

pattern Image :: Image -> Image
pattern Image i = i

instance Pure Image where
    render Image_ {..} =
        let
            cs =
                ( size # "ui"
                : size
                : avatar # "avatar"
                : bordered # "bordered"
                : circular # "circular"
                : centered # "centered"
                : disabled # "disabled"
                : fluid # "fluid"
                : hidden # "hidden"
                : inline # "inline"
                : rounded # "rounded"
                : useKeyOrValueAndKey spaced "spaced"
                : floated # ("floated" <<>> floated)
                : verticalAlign # ("aligned" <<>> verticalAlign)
                : "image"
                )
        in
            as
                : attributes
                )
                children

instance HasProp Avatar Image where
    type Prop Avatar Image = Bool
    getProp _ = avatar
    setProp _ a i = i { avatar = a }

instance HasProp As Image where
    type Prop As Image = Features -> [View] -> View
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasFeatures Image where
    getFeatures = features
    setFeatures cs i = i { features = cs }

instance HasProp Bordered Image where
    type Prop Bordered Image = Bool
    getProp _ = bordered
    setProp _ b i = i { bordered = b }

instance HasProp Centered Image where
    type Prop Centered Image = Bool
    getProp _ = centered
    setProp _ c i = i { centered = c }

instance HasChildren Image where
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasProp Circular Image where
    type Prop Circular Image = Bool
    getProp _ = circular
    setProp _ c i = i { circular = c }


instance HasProp Disabled Image where
    type Prop Disabled Image = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Floated Image where
    type Prop Floated Image = Txt
    getProp _ = floated
    setProp _ f i = i { floated = f }

instance HasProp Fluid Image where
    type Prop Fluid Image = Bool
    getProp _ = fluid
    setProp _ f i = i { fluid = f }

instance HasProp Inline Image where
    type Prop Inline Image = Bool
    getProp _ = inline
    setProp _ inl i = i { inline = inl }

instance HasProp Hidden Image where
    type Prop Hidden Image = Bool
    getProp _ = hidden
    setProp _ h i = i { hidden = h }

instance HasProp Rounded Image where
    type Prop Rounded Image = Bool
    getProp _ = rounded
    setProp _ r i = i { rounded = r }

instance HasProp Size Image where
    type Prop Size Image = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

instance HasProp Spaced Image where
    type Prop Spaced Image = Maybe Txt
    getProp _ = spaced
    setProp _ s i = i { spaced = s }

instance HasProp VerticalAlign Image where
    type Prop VerticalAlign Image = Txt
    getProp _ = verticalAlign
    setProp _ va i = i { verticalAlign = va }

instance HasProp Wrapped Image where
    type Prop Wrapped Image = Bool
    getProp _ = wrapped
    setProp _ w i = i { wrapped = w }

data Meta = Meta_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Meta where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Meta :: Meta -> Meta
pattern Meta im = im

instance Pure Meta where
    render Meta_ {..} =
        let
            cs =
                ( "meta"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Meta where
    type Prop As Meta = Features -> [View] -> View
    getProp _ = as
    setProp _ a im = im { as = a }

instance HasFeatures Meta where
    getFeatures = features
    setFeatures as im = im { features = as }

instance HasChildren Meta where
    getChildren = children
    setChildren cs im = im { children = cs }

