module Semantic.Message
  ( module Properties
  , module Tools
  , Message(..), pattern Message
  , Content(..), pattern Content
  , Header(..), pattern Semantic.Message.Header
  , Item(..), pattern Item
  , List(..), pattern List
  ) where

import Pure hiding (Content,Content_,Icon,List,Name,color,hidden,visible)

import GHC.Generics as G

import Semantic.Utils

import Semantic.Icon

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Name, Name(..)
  , pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Color, Color(..)
  , pattern Compact, Compact(..)
  , pattern Error, Error(..)
  , pattern Floating, Floating(..)
  , pattern Hidden, Hidden(..)
  , pattern Info, Info(..)
  , pattern Negative, Negative(..)
  , pattern OnDismiss, OnDismiss(..)
  , pattern Positive, Positive(..)
  , pattern Size, Size(..)
  , pattern Success, Success(..)
  , pattern Visible, Visible(..)
  , pattern Warning, Warning(..)
  )

import Prelude hiding (error,Floating)

import Data.Function as Tools ((&))

data Message = Message_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , attached :: Maybe Txt
    , color :: Txt
    , compact :: Bool
    , error :: Bool
    , floating :: Bool
    , hidden :: Bool
    , info :: Bool
    , negative :: Bool
    , onDismiss :: Maybe (IO ())
    , positive :: Bool
    , size :: Txt
    , success :: Bool
    , visible :: Bool
    , warning :: Bool
    } deriving (Generic)

instance Default Message where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Message :: Message -> Message
pattern Message m = m

instance Pure Message where
    view Message_ {..} =
        let
            icon = foldPures (\(View (Icon_ {})) -> const True) False children

            dismissIcon =
              case onDismiss of
                Nothing -> Null
                Just od ->
                  View $ Icon $ def & Name "close" & OnClick (\_ -> od)

            cs =
                [ "ui"
                , color
                , size
                , compact # "compact"
                , error # "error"
                , floating # "floating"
                , hidden # "hidden"
                , icon # "icon"
                , info # "info"
                , negative # "negative"
                , positive # "positive"
                , success # "success"
                , visible # "visible"
                , warning # "warning"
                , maybe "" (<>> "attached") attached
                , "message"
                ]
        in
            as (features & Classes cs) (dismissIcon : children)

instance HasProp As Message where
    type Prop As Message = Features -> [View] -> View
    getProp _ = as
    setProp _ a m = m { as = a }

instance HasFeatures Message where
    getFeatures = features
    setFeatures as m = m { features = as }

instance HasChildren Message where
    getChildren = children
    setChildren cs m = m { children = cs }

instance HasProp Attached Message where
    type Prop Attached Message = Maybe Txt
    getProp _ = attached
    setProp _ a m = m { attached = a }

instance HasProp Color Message where
    type Prop Color Message = Txt
    getProp _ = color
    setProp _ c m = m { color = c }

instance HasProp Compact Message where
    type Prop Compact Message = Bool
    getProp _ = compact
    setProp _ c m = m { compact = c }

instance HasProp Error Message where
    type Prop Error Message = Bool
    getProp _ = error
    setProp _ e m = m { error = e }

instance HasProp Floating Message where
    type Prop Floating Message = Bool
    getProp _ = floating
    setProp _ f m = m { floating = f }

instance HasProp Hidden Message where
    type Prop Hidden Message = Bool
    getProp _ = hidden
    setProp _ h m = m { hidden = h }

instance HasProp Info Message where
    type Prop Info Message = Bool
    getProp _ = info
    setProp _ i m = m { info = i }

instance HasProp Negative Message where
    type Prop Negative Message = Bool
    getProp _ = negative
    setProp _ n m = m { negative = n }

instance HasProp OnDismiss Message where
    type Prop OnDismiss Message = Maybe (IO ())
    getProp _ = onDismiss
    setProp _ od m = m { onDismiss = od }

instance HasProp Positive Message where
    type Prop Positive Message = Bool
    getProp _ = positive
    setProp _ p m = m { positive = p }

instance HasProp Size Message where
    type Prop Size Message = Txt
    getProp _ = size
    setProp _ s m = m { size = s }

instance HasProp Success Message where
    type Prop Success Message = Bool
    getProp _ = success
    setProp _ s m = m { success = s }

instance HasProp Visible Message where
    type Prop Visible Message = Bool
    getProp _ = visible
    setProp _ v m = m { visible = v }

instance HasProp Warning Message where
    type Prop Warning Message = Bool
    getProp _ = warning
    setProp _ w m = m { warning = w }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content mc = mc

instance Pure Content where
    view Content_ {..} = as (features & Class "content") children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ a mc = mc { as = a }

instance HasFeatures Content where
    getFeatures = features
    setFeatures as mc = mc { features = as }

instance HasChildren Content where
    getChildren = children
    setChildren cs mc = mc { children = cs }

data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header mh = mh

instance Pure Header where
    view Header_ {..} = as (features & Class "header") children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ a mh = mh { as = a }

instance HasFeatures Header where
    getFeatures = features
    setFeatures as mh = mh { features = as }

instance HasChildren Header where
    getChildren = children
    setChildren cs mh = mh { children = cs }

data Item = Item_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Item where
    def = (G.to gdef) { as = \fs cs -> Li & Features fs & Children cs }

pattern Item :: Item -> Item
pattern Item mi = mi

instance Pure Item where
    view Item_ {..} = as (features & Class "content") children

instance HasProp As Item where
    type Prop As Item = Features -> [View] -> View
    getProp _ = as
    setProp _ a mi = mi { as = a }

instance HasFeatures Item where
    getFeatures = features
    setFeatures as mi = mi { features = as }

instance HasChildren Item where
    getChildren = children
    setChildren cs mi = mi { children = cs }

data List = List_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default List where
    def = (G.to gdef) { as = \fs cs -> Ul & Features fs & Children cs }

pattern List :: List -> List
pattern List ml = ml

instance Pure List where
    view List_ {..} = as (features & Class "list") children

instance HasProp As List where
    type Prop As List = Features -> [View] -> View
    getProp _ = as
    setProp _ a ml = ml { as = a }

instance HasFeatures List where
    getFeatures = features
    setFeatures as ml = ml { features = as }

instance HasChildren List where
    getChildren = children
    setChildren cs ml = ml { children = cs }

