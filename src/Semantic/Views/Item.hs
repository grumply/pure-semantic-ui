module Semantic.Views.Item
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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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

import Data.Function as Tool ((&))
import Pure.Data.Default as Tools

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Item ms -> View ms
pattern Item i = View i

instance Pure Item ms where
    render Item_ {..} =
        let
            cs =
                ( "item"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Item ms) where
    type Prop As (Item ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a i = i { as = a }

instance HasProp Attributes (Item ms) where
    type Prop Attributes (Item ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as i = i { attributes = as }

instance HasProp Children (Item ms) where
    type Prop Children (Item ms) = [View ms]
    getProp _ = children
    setProp _ cs i = i { children = cs }

instance HasProp Classes (Item ms) where
    type Prop Classes (Item ms) = [Txt]
    getProp _ = classes
    setProp _ cs i = i { classes = cs }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content ic = View ic

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( verticalAlign
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
    setProp _ a ic = ic { as = a }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ic = ic { attributes = as }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs ic = ic { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs ic = ic { classes = cs }

instance HasProp VerticalAlign (Content ms) where
    type Prop VerticalAlign (Content ms) = Txt
    getProp _ = verticalAlign
    setProp _ va ic = ic { verticalAlign = va }

data Description ms = Description_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Description ms) where
    def = (G.to gdef) { as = Div }

pattern Description :: Description ms -> View ms
pattern Description id = View id

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
    setProp _ a id = id { as = a }

instance HasProp Attributes (Description ms) where
    type Prop Attributes (Description ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as id = id { attributes = as }

instance HasProp Children (Description ms) where
    type Prop Children (Description ms) = [View ms]
    getProp _ = children
    setProp _ cs id = id { children = cs }

instance HasProp Classes (Description ms) where
    type Prop Classes (Description ms) = [Txt]
    getProp _ = classes
    setProp _ cs id = id { classes = cs }

data Extra ms = Extra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Extra ms) where
    def = (G.to gdef) { as = Div }

pattern Extra :: Extra ms -> View ms
pattern Extra ie = View ie

instance Pure Extra ms where
    render Extra_ {..} =
        let
            cs =
                ( "extra"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Extra ms) where
    type Prop As (Extra ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ie = ie { as = a }

instance HasProp Attributes (Extra ms) where
    type Prop Attributes (Extra ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ie = ie { attributes = as }

instance HasProp Children (Extra ms) where
    type Prop Children (Extra ms) = [View ms]
    getProp _ = children
    setProp _ cs ie = ie { children = cs }

instance HasProp Classes (Extra ms) where
    type Prop Classes (Extra ms) = [Txt]
    getProp _ = classes
    setProp _ cs ie = ie { classes = cs }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , divided :: Bool
    , link :: Bool
    , relaxed :: Maybe Txt
    , unstackable :: Bool
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group ig = View ig

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : divided # "divided"
                : link # "link"
                : unstackable # "unstackable"
                : may (<>> "relaxed") relaxed
                : "items"
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
    setProp _ a ig = ig { as = a }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ig = ig { attributes = as }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs ig = ig { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs ig = ig { classes = cs }

instance HasProp Divided (Group ms) where
    type Prop Divided (Group ms) = Bool
    getProp _ = divided
    setProp _ d ig = ig { divided = d }

instance HasProp Link (Group ms) where
    type Prop Link (Group ms) = Bool
    getProp _ = link
    setProp _ l ig = ig { link = l }

instance HasProp Relaxed (Group ms) where
    type Prop Relaxed (Group ms) = Maybe Txt
    getProp _ = relaxed
    setProp _ r ig = ig { relaxed = r }

instance HasProp Unstackable (Group ms) where
    type Prop Unstackable (Group ms) = Bool
    getProp _ = unstackable
    setProp _ u ig = ig { unstackable = u }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header ih = View ih

instance Pure Header ms where
    render Header_ {..} =
        let
            cs =
                ( "header"
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
    setProp _ a ih = ih { as = a }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ih = ih { attributes = as }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs ih = ih { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs ih = ih { classes = cs }

data Image ms = Image_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , avatar :: Bool
    , bordered :: Bool
    , centered :: Bool
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
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

instance Default (Image ms) where
    def = (G.to gdef) { as = Img }

pattern Image :: Image ms -> View ms
pattern Image i = View i

instance Pure Image ms where
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
                : "Image"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp Avatar (Image ms) where
    type Prop Avatar (Image ms) = Bool
    getProp _ = avatar
    setProp _ a i = i { avatar = a }

instance HasProp As (Image ms) where
    type Prop As (Image ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f i = i { as = f }

instance HasProp Attributes (Image ms) where
    type Prop Attributes (Image ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs i = i { attributes = cs }

instance HasProp Bordered (Image ms) where
    type Prop Bordered (Image ms) = Bool
    getProp _ = bordered
    setProp _ b i = i { bordered = b }

instance HasProp Centered (Image ms) where
    type Prop Centered (Image ms) = Bool
    getProp _ = centered
    setProp _ c i = i { centered = c }

instance HasProp Children (Image ms) where
    type Prop Children (Image ms) = [View ms]
    getProp _ = children
    setProp _ cs i = i { children = cs }

instance HasProp Circular (Image ms) where
    type Prop Circular (Image ms) = Bool
    getProp _ = circular
    setProp _ c i = i { circular = c }

instance HasProp Classes (Image ms) where
    type Prop Classes (Image ms) = [Txt]
    getProp _ = classes
    setProp _ cs i = i { classes = cs }

instance HasProp Disabled (Image ms) where
    type Prop Disabled (Image ms) = Bool
    getProp _ = disabled
    setProp _ d i = i { disabled = d }

instance HasProp Floated (Image ms) where
    type Prop Floated (Image ms) = Txt
    getProp _ = floated
    setProp _ f i = i { floated = f }

instance HasProp Fluid (Image ms) where
    type Prop Fluid (Image ms) = Bool
    getProp _ = fluid
    setProp _ f i = i { fluid = f }

instance HasProp Inline (Image ms) where
    type Prop Inline (Image ms) = Bool
    getProp _ = inline
    setProp _ inl i = i { inline = inl }

instance HasProp Hidden (Image ms) where
    type Prop Hidden (Image ms) = Bool
    getProp _ = hidden
    setProp _ h i = i { hidden = h }

instance HasProp Rounded (Image ms) where
    type Prop Rounded (Image ms) = Bool
    getProp _ = rounded
    setProp _ r i = i { rounded = r }

instance HasProp Size (Image ms) where
    type Prop Size (Image ms) = Txt
    getProp _ = size
    setProp _ s i = i { size = s }

instance HasProp Spaced (Image ms) where
    type Prop Spaced (Image ms) = Maybe Txt
    getProp _ = spaced
    setProp _ s i = i { spaced = s }

instance HasProp VerticalAlign (Image ms) where
    type Prop VerticalAlign (Image ms) = Txt
    getProp _ = verticalAlign
    setProp _ va i = i { verticalAlign = va }

instance HasProp Wrapped (Image ms) where
    type Prop Wrapped (Image ms) = Bool
    getProp _ = wrapped
    setProp _ w i = i { wrapped = w }

data Meta ms = Meta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Meta ms) where
    def = (G.to gdef) { as = Div }

pattern Meta :: Meta ms -> View ms
pattern Meta im = View im

instance Pure Meta ms where
    render Meta_ {..} =
        let
            cs =
                ( "meta"
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
    setProp _ a im = im { as = a }

instance HasProp Attributes (Meta ms) where
    type Prop Attributes (Meta ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as im = im { attributes = as }

instance HasProp Children (Meta ms) where
    type Prop Children (Meta ms) = [View ms]
    getProp _ = children
    setProp _ cs im = im { children = cs }

instance HasProp Classes (Meta ms) where
    type Prop Classes (Meta ms) = [Txt]
    getProp _ = classes
    setProp _ cs im = im { classes = cs }
