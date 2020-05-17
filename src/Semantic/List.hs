{-# LANGUAGE UndecidableInstances #-}
module Semantic.List
  ( module Properties
  , module Tools
  , List(..), pattern Semantic.List.List
  , Content(..), pattern Semantic.List.Content
  , Description(..), pattern Semantic.List.Description
  , Header(..), pattern Semantic.List.Header
  , Icon(..), pattern Semantic.List.Icon
  , Item(..), pattern Item
  , Sublist(..), pattern Sublist
  , Keyed(..), pattern Semantic.List.Keyed
  ) where

import Pure hiding (Content_,color,horizontal,size,not,corner,name,link,disabled,active,(#))

import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Animated, Animated(..)
  , pattern As, As(..)
  , pattern Bulleted, Bulleted(..)
  , pattern Celled, Celled(..)
  , pattern Divided, Divided(..)
  , pattern Floated, Floated(..)
  , pattern Horizontal, Horizontal(..)
  , pattern Inverted, Inverted(..)
  , pattern Link, Link(..)
  , pattern Ordered, Ordered(..)
  , pattern Relaxed, Relaxed(..)
  , pattern Selection, Selection(..)
  , pattern Size, Size(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Bordered, Bordered(..)
  , pattern Circular, Circular(..)
  , pattern Color, Color(..)
  , pattern Corner, Corner(..)
  , pattern Disabled, Disabled(..)
  , pattern Fitted, Fitted(..)
  , pattern Flipped, Flipped(..)
  , pattern Loading, Loading(..)
  , pattern Name, Name(..)
  , pattern Rotated, Rotated(..)
  , pattern Active, Active(..)
  , pattern Value, Value(..)
  )

import Data.Function as Tools ((&))

data List = List_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , animated :: Bool
    , bulleted :: Bool
    , celled :: Bool
    , divided :: Bool
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , link :: Bool
    , ordered :: Bool
    , relaxed :: Maybe Txt
    , selection :: Bool
    , size :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default List where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern List :: List -> List
pattern List l = l

instance Pure List where
    view List_ {..} =
        let
            cs =
                [ "ui"
                , size
                , animated # "animated"
                , bulleted # "bulleted"
                , celled # "celled"
                , divided # "divided"
                , horizontal # "horizontal"
                , inverted # "inverted"
                , link # "link"
                , ordered # "ordered"
                , selection # "selection"
                , maybe "" (<>> "relaxed") relaxed
                , (floated /= mempty) # (floated <>> "floated")
                , (verticalAlign /= mempty) # (verticalAlign <>> "aligned")
                , "list"
                ]
        in
            as (features & Classes cs) children

instance HasProp Animated List where
    type Prop Animated List = Bool
    getProp _ = animated
    setProp _ anim l = l { animated = anim }

instance HasProp As List where
    type Prop As List = Features -> [View] -> View
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasFeatures List where
    getFeatures = features
    setFeatures cs l = l { features = cs }

instance HasProp Bulleted List where
    type Prop Bulleted List = Bool
    getProp _ = bulleted
    setProp _ b l = l { bulleted = b }

instance HasProp Celled List where
    type Prop Celled List = Bool
    getProp _ = celled
    setProp _ c l = l { celled = c }

instance HasChildren List where
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasProp Divided List where
    type Prop Divided List = Bool
    getProp _ = divided
    setProp _ d l = l { divided = d }

instance HasProp Floated List where
    type Prop Floated List = Txt
    getProp _ = floated
    setProp _ f l = l { floated = f }

instance HasProp Horizontal List where
    type Prop Horizontal List = Bool
    getProp _ = horizontal
    setProp _ h l = l { horizontal = h }

instance HasProp Inverted List where
    type Prop Inverted List = Bool
    getProp _ = inverted
    setProp _ i l = l { inverted = i }

instance HasProp Link List where
    type Prop Link List = Bool
    getProp _ = link
    setProp _ lnk l = l { link = lnk }

instance HasProp Ordered List where
    type Prop Ordered List = Bool
    getProp _ = ordered
    setProp _ o l = l { ordered = o }

instance HasProp Relaxed List where
    type Prop Relaxed List = Maybe Txt
    getProp _ = relaxed
    setProp _ r l = l { relaxed = r }

instance HasProp Selection List where
    type Prop Selection List = Bool
    getProp _ = selection
    setProp _ s l = l { selection = s }

instance HasProp Size List where
    type Prop Size List = Txt
    getProp _ = size
    setProp _ s l = l { size = s }

instance HasProp VerticalAlign List where
    type Prop VerticalAlign List = Txt
    getProp _ = verticalAlign
    setProp _ va l = l { verticalAlign = va }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , floated :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content lc = lc

instance Pure Content where
    view Content_ {..} =
        let
            cs =
                [ (floated /= mempty) # (floated <>> "floated")
                , (verticalAlign /= mempty) # (verticalAlign <>> "aligned")
                , "content"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ f lc = lc { as = f }

instance HasFeatures Content where
    getFeatures = features
    setFeatures cs lc = lc { features = cs }

instance HasChildren Content where
    getChildren = children
    setChildren cs lc = lc { children = cs }

instance HasProp Floated Content where
    type Prop Floated Content = Txt
    getProp _ = floated
    setProp _ f lc = lc { floated = f }

instance HasProp VerticalAlign Content where
    type Prop VerticalAlign Content = Txt
    getProp _ = verticalAlign
    setProp _ va lc = lc { verticalAlign = va }

data Description = Description_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Description where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Description :: Description -> Description
pattern Description ld = ld

instance Pure Description where
    view Description_ {..} = as (features & Class "description") children

instance HasProp As Description where
    type Prop As Description = Features -> [View] -> View
    getProp _ = as
    setProp _ f ld = ld { as = f }

instance HasFeatures Description where
    getFeatures = features
    setFeatures cs ld = ld { features = cs }

instance HasChildren Description where
    getChildren = children
    setChildren cs ld = ld { children = cs }

data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header lh = lh

instance Pure Header where
    view Header_ {..} = as (features & Class "header") children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ f lh = lh { as = f }

instance HasFeatures Header where
    getFeatures = features
    setFeatures cs lh = lh { features = cs }

instance HasChildren Header where
    getChildren = children
    setChildren cs lh = lh { children = cs }

data Icon = Icon_
    { as :: Features -> [View] -> View
    , features :: Features
    , bordered :: Bool
    , circular :: Bool
    , color :: Txt
    , corner :: Bool
    , disabled :: Bool
    , fitted :: Bool
    , flipped :: Txt
    , inverted :: Bool
    , link :: Bool
    , loading :: Bool
    , name :: Txt
    , rotated :: Txt
    , size :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Icon where
    def = (G.to gdef) { as = \fs cs -> I & Features fs & Children cs }

pattern Icon :: Icon -> Icon
pattern Icon li = li

instance Pure Icon where
    view Icon_ {..} =
        let
            cs =
                [ color
                , name
                , (size /= mempty) # size
                , bordered # "bordered"
                , circular # "circular"
                , corner # "corner"
                , disabled # "disabled"
                , fitted # "fitted"
                , inverted # "inverted"
                , link # "link"
                , loading # "loading"
                , (flipped /= mempty) # ("flipped" <<>> flipped)
                , (rotated /= mempty) # ("rotated" <<>> rotated)
                , (verticalAlign /= mempty) # (verticalAlign <>> "aligned")
                , "icon"
                ]
        in
            as (features & Classes cs) []

instance HasProp As Icon where
    type Prop As Icon = Features -> [View] -> View
    getProp _ = as
    setProp _ f li = li { as = f }

instance HasFeatures Icon where
    getFeatures = features
    setFeatures cs li = li { features = cs }

instance HasProp Bordered Icon where
    type Prop Bordered Icon = Bool
    getProp _ = bordered
    setProp _ b li = li { bordered = b }

instance HasProp Circular Icon where
    type Prop Circular Icon = Bool
    getProp _ = circular
    setProp _ c li = li { circular = c }

instance HasProp Name Icon where
    type Prop Name Icon = Txt
    getProp _ = name
    setProp _ n li = li { name = n }

instance HasProp Color Icon where
    type Prop Color Icon = Txt
    getProp _ = color
    setProp _ c li = li { color = c }

instance HasProp Corner Icon where
    type Prop Corner Icon = Bool
    getProp _ = corner
    setProp _ c li = li { corner = c }

instance HasProp Disabled Icon where
    type Prop Disabled Icon = Bool
    getProp _ = disabled
    setProp _ d li = li { disabled = d }

instance HasProp Fitted Icon where
    type Prop Fitted Icon = Bool
    getProp _ = fitted
    setProp _ f li = li { fitted = f }

instance HasProp Flipped Icon where
    type Prop Flipped Icon = Txt
    getProp _ = flipped
    setProp _ f li = li { flipped = f }

instance HasProp Inverted Icon where
    type Prop Inverted Icon = Bool
    getProp _ = inverted
    setProp _ i li = li { inverted = i }

instance HasProp Link Icon where
    type Prop Link Icon = Bool
    getProp _ = link
    setProp _ l li = li { link = l }

instance HasProp Loading Icon where
    type Prop Loading Icon = Bool
    getProp _ = loading
    setProp _ l li = li { loading = l }

instance HasProp Rotated Icon where
    type Prop Rotated Icon = Txt
    getProp _ = rotated
    setProp _ r li = li { rotated = r }

instance HasProp Size Icon where
    type Prop Size Icon = Txt
    getProp _ = size
    setProp _ s li = li { size = s }

instance HasProp VerticalAlign Icon where
    type Prop VerticalAlign Icon = Txt
    getProp _ = verticalAlign
    setProp _ va li = li { verticalAlign = va }

data Item = Item_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , disabled :: Bool
    , value :: Txt
    } deriving (Generic)

instance Default Item where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Item :: Item -> Item
pattern Item li = li

instance Pure Item where
    view Item_ {..} =
        let
            li =
                case as mempty [] of
                    HTMLView _ "li" _ _ -> True
                    _ -> False
            cs =
                [ active # "active"
                , disabled # "disabled"
                , (not li) # "item"
                ]

            valueProp = li ? Pure.Value value $ Property "data-value" value
        in
            as (features & Classes cs & valueProp & Role "listitem") children

instance HasProp Active Item where
    type Prop Active Item = Bool
    getProp _ = active
    setProp _ a li = li { active = a }

instance HasProp As Item where
    type Prop As Item = Features -> [View] -> View
    getProp _ = as
    setProp _ f li = li { as = f }

instance HasFeatures Item where
    getFeatures = features
    setFeatures cs li = li { features = cs }

instance HasChildren Item where
    getChildren = children
    setChildren cs li = li { children = cs }

instance HasProp Disabled Item where
    type Prop Disabled Item = Bool
    getProp _ = disabled
    setProp _ d li = li { disabled = d }

instance HasProp Value Item where
    type Prop Value Item = Txt
    getProp _ = value
    setProp _ v li = li { value = v }

data Sublist = Sublist_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Sublist where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Sublist :: Sublist -> Sublist
pattern Sublist ll = ll

instance Pure Sublist where
    view Sublist_ {..} =
        let
            proxy =
                case as mempty [] of
                    HTMLView _ "ul" _ _ -> False
                    HTMLView _ "ol" _ _ -> False
                    _ -> True

        in
            as (features & (if proxy then Class "list" else id)) children

instance HasProp As Sublist where
    type Prop As Sublist = Features -> [View] -> View
    getProp _ = as
    setProp _ f ll = ll { as = f }

instance HasFeatures Sublist where
    getFeatures = features
    setFeatures cs ll = ll { features = cs }

instance HasChildren Sublist where
    getChildren = children
    setChildren cs ll = ll { children = cs }

data Keyed = Keyed_
    { as :: Features -> [(Int,View)] -> View
    , features :: Features
    , children :: [(Int,View)]
    , animated :: Bool
    , bulleted :: Bool
    , celled :: Bool
    , divided :: Bool
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , link :: Bool
    , onItemClick :: (Int,Item) -> IO ()
    , ordered :: Bool
    , relaxed :: Maybe Txt
    , selection :: Bool
    , size :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default Keyed where
    def = (G.to gdef) { as = \fs cs -> (Pure.Keyed Div) & Features fs & KeyedChildren cs }

pattern Keyed :: Keyed -> Keyed
pattern Keyed l = l

instance Pure Keyed where
    view Keyed_ {..} =
        let
            cs =
                [ "ui"
                , size
                , animated # "animated"
                , bulleted # "bulleted"
                , celled # "celled"
                , divided # "divided"
                , horizontal # "horizontal"
                , inverted # "inverted"
                , link # "link"
                , ordered # "ordered"
                , selection # "selection"
                , maybe "" (<>> "relaxed") relaxed
                , (floated /= mempty) # (floated <>> "floated")
                , (verticalAlign /= mempty) # (verticalAlign <>> "aligned")
                , "list"
                ]
        in
            as (features & Classes cs) children

instance HasProp Animated Keyed where
    type Prop Animated Keyed = Bool
    getProp _ = animated
    setProp _ anim l = l { animated = anim }

instance HasProp As Keyed where
    type Prop As Keyed = Features -> [(Int,View)] -> View
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasFeatures Keyed where
    getFeatures = features
    setFeatures cs l = l { features = cs }

instance HasProp Bulleted Keyed where
    type Prop Bulleted Keyed = Bool
    getProp _ = bulleted
    setProp _ b l = l { bulleted = b }

instance HasProp Celled Keyed where
    type Prop Celled Keyed = Bool
    getProp _ = celled
    setProp _ c l = l { celled = c }

instance HasKeyedChildren Keyed where
    getKeyedChildren = children
    setKeyedChildren cs l = l { children = cs }

instance HasProp Divided Keyed where
    type Prop Divided Keyed = Bool
    getProp _ = divided
    setProp _ d l = l { divided = d }

instance HasProp Floated Keyed where
    type Prop Floated Keyed = Txt
    getProp _ = floated
    setProp _ f l = l { floated = f }

instance HasProp Horizontal Keyed where
    type Prop Horizontal Keyed = Bool
    getProp _ = horizontal
    setProp _ h l = l { horizontal = h }

instance HasProp Inverted Keyed where
    type Prop Inverted Keyed = Bool
    getProp _ = inverted
    setProp _ i l = l { inverted = i }

instance HasProp Link Keyed where
    type Prop Link Keyed = Bool
    getProp _ = link
    setProp _ lnk l = l { link = lnk }

instance HasProp Ordered Keyed where
    type Prop Ordered Keyed = Bool
    getProp _ = ordered
    setProp _ o l = l { ordered = o }

instance HasProp Relaxed Keyed where
    type Prop Relaxed Keyed = Maybe Txt
    getProp _ = relaxed
    setProp _ r l = l { relaxed = r }

instance HasProp Selection Keyed where
    type Prop Selection Keyed = Bool
    getProp _ = selection
    setProp _ s l = l { selection = s }

instance HasProp Size Keyed where
    type Prop Size Keyed = Txt
    getProp _ = size
    setProp _ s l = l { size = s }

instance HasProp VerticalAlign Keyed where
    type Prop VerticalAlign Keyed = Txt
    getProp _ = verticalAlign
    setProp _ va l = l { verticalAlign = va }
