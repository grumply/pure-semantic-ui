module Semantic.Feed
  ( module Properties
  , module Tools
  , Feed(..), pattern Feed
  , Content(..), pattern Content
  , Date(..), pattern Date
  , Event(..), pattern Event
  , Extra(..), pattern Extra
  , Label(..), pattern Label
  , Like(..), pattern Like
  , Meta(..), pattern Meta
  , User(..), pattern User
  ) where

import GHC.Generics as G hiding (Meta)
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML

import Semantic.Utils

import Semantic.Image (Image(..))

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Size, Size(..)
  , pattern IsText, IsText(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Feed = Feed_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , size :: Txt
    } deriving (Generic)

instance Default Feed where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Feed :: Feed -> Feed
pattern Feed f = f

instance Pure Feed where
    view Feed_ {..} =
        let
            cs =
                [ "ui"
                , size
                , "feed"
                ]
        in
            as
                : attributes
                )
                children

instance HasProp As Feed where
    type Prop As Feed = Features -> [View] -> View
    getProp _ = as
    setProp _ a f = f { as = a }

instance HasFeatures Feed where
    getFeatures = features
    setFeatures as f = f { features = as }

instance HasChildren Feed where
    getChildren = children
    setChildren cs f = f { children = cs }

instance HasProp Size Feed where
    type Prop Size Feed = Txt
    getProp _ = size
    setProp _ s f = f { size = s }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content fc = fc

instance Pure Content where
    view Content_ {..} =
        let
            cs =
                ( "content"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ a fc = fc { as = a }

instance HasFeatures Content where
    getFeatures = features
    setFeatures as fc = fc { features = as }

instance HasChildren Content where
    getChildren = children
    setChildren cs fc = fc { children = cs }

data Date = Date_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Date where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Date :: Date -> Date
pattern Date fd = fd

instance Pure Date where
    view Date_ {..} =
        let
            cs =
                ( "date"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Date where
    type Prop As Date = Features -> [View] -> View
    getProp _ = as
    setProp _ a fd = fd { as = a }

instance HasFeatures Date where
    getFeatures = features
    setFeatures as fd = fd { features = as }

instance HasChildren Date where
    getChildren = children
    setChildren cs fd = fd { children = cs }

data Event = Event_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Event where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Event :: Event -> Event
pattern Event fe = fe

instance Pure Event where
    view Event_ {..} =
        let
            cs =
                ( "event"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Event where
    type Prop As Event = Features -> [View] -> View
    getProp _ = as
    setProp _ a fe = fe { as = a }

instance HasFeatures Event where
    getFeatures = features
    setFeatures as fe = fe { features = as }

instance HasChildren Event where
    getChildren = children
    setChildren cs fe = fe { children = cs }

data Extra = Extra_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , text :: Bool
    } deriving (Generic)

instance Default Extra where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Extra :: Extra -> Extra
pattern Extra fe = fe

instance Pure Extra where
    view Extra_ {..} =
        let
            image = foldPures (\(Image_ {}) -> const True) False children

            cs =
                [ image # "images"
                , text # "text"
                , "extra"
                ]
        in
            as
                : attributes
                )
                children

instance HasProp As Extra where
    type Prop As Extra = Features -> [View] -> View
    getProp _ = as
    setProp _ a fe = fe { as = a }

instance HasFeatures Extra where
    getFeatures = features
    setFeatures as fe = fe { features = as }

instance HasChildren Extra where
    getChildren = children
    setChildren cs fe = fe { children = cs }

instance HasProp IsText Extra where
    type Prop IsText Extra = Bool
    getProp _ = text
    setProp _ it fe = fe { text = it }

data Label = Label_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Label where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Label :: Label -> Label
pattern Label fl = fl

instance Pure Label where
    view Label_ {..} =
        let
            cs =
                ( "label"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Label where
    type Prop As Label = Features -> [View] -> View
    getProp _ = as
    setProp _ a fl = fl { as = a }

instance HasFeatures Label where
    getFeatures = features
    setFeatures as fl = fl { features = as }

instance HasChildren Label where
    getChildren = children
    setChildren cs fl = fl { children = cs }

data Like = Like_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Like where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Like :: Like -> Like
pattern Like fl = fl

instance Pure Like where
    view Like_ {..} =
        let
            cs =
                ( "like"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Like where
    type Prop As Like = Features -> [View] -> View
    getProp _ = as
    setProp _ a fl = fl { as = a }

instance HasFeatures Like where
    getFeatures = features
    setFeatures as fl = fl { features = as }

instance HasChildren Like where
    getChildren = children
    setChildren cs fl = fl { children = cs }

data Meta = Meta_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Meta where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Meta :: Meta -> Meta
pattern Meta fm = fm

instance Pure Meta where
    view Meta_ {..} =
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
    setProp _ a fm = fm { as = a }

instance HasFeatures Meta where
    getFeatures = features
    setFeatures as fm = fm { features = as }

instance HasChildren Meta where
    getChildren = children
    setChildren cs fm = fm { children = cs }

data Summary = Summary_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Summary where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Summary :: Summary -> Summary
pattern Summary fs = fs

instance Pure Summary where
    view Summary_ {..} =
        let
            cs =
                ( "summary"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Summary where
    type Prop As Summary = Features -> [View] -> View
    getProp _ = as
    setProp _ a fs = fs { as = a }

instance HasFeatures Summary where
    getFeatures = features
    setFeatures as fs = fs { features = as }

instance HasChildren Summary where
    getChildren = children
    setChildren cs fs = fs { children = cs }

data User = User_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default User where
    def = (G.to gdef) { as = \fs cs -> A & Features fs & Children cs }

pattern User :: User -> User
pattern User fu = fu

instance Pure User where
    view User_ {..} =
        let
            cs =
                ( "user"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As User where
    type Prop As User = Features -> [View] -> View
    getProp _ = as
    setProp _ a fu = fu { as = a }

instance HasFeatures User where
    getFeatures = features
    setFeatures as fu = fu { features = as }

instance HasChildren User where
    getChildren = children
    setChildren cs fu = fu { children = cs }

