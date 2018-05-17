module Semantic.Comment
  ( module Properties
  , module Tools
  , Comment(..), pattern Comment
  , Action(..), pattern Action
  , Actions(..), pattern Actions
  , Author(..), pattern Author
  , Avatar(..), pattern Avatar
  , Content(..), pattern Content
  , Group(..), pattern Group
  , Metadata(..), pattern Metadata
  , Text(..), pattern Text
  ) where

import GHC.Generics as G
import Pure.View hiding (Action,active,Content,Text)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Collapsed, Collapsed(..)
  , pattern Active, Active(..)
  , pattern Minimal, Minimal(..)
  , pattern Size, Size(..)
  , pattern Threaded, Threaded(..)
  , pattern Src, Src(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Comment = Comment_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , collapsed :: Bool
    } deriving (Generic)

instance Default Comment where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Comment :: Comment -> Comment
pattern Comment a = a

instance Pure Comment where
    render Comment_ {..} =
        let
            cs =
                ( collapsed # "collapsed"
                : "comment"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Comment where
    type Prop As Comment = Features -> [View] -> View
    getProp _ = as
    setProp _ a c = c { as = a }

instance HasFeatures Comment where
    getFeatures = features
    setFeatures as c = c { features = as }

instance HasChildren Comment where
    getChildren = children
    setChildren cs c = c { children = cs }


instance HasProp Collapsed Comment where
    type Prop Collapsed Comment = Bool
    getProp _ = collapsed
    setProp _ c com = com { collapsed = c }

data Action = Action_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    } deriving (Generic)

instance Default Action where
    def = (G.to gdef) { as = \fs cs -> A & Features fs & Children cs }

pattern Action :: Action -> Action
pattern Action ca = ca

instance Pure Action where
    render Action_ {..} =
        let
            cs =
                ( active # "active"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Action where
    type Prop As Action = Features -> [View] -> View
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasFeatures Action where
    getFeatures = features
    setFeatures as ca = ca { features = as }

instance HasChildren Action where
    getChildren = children
    setChildren cs ca = ca { children = cs }


instance HasProp Active Action where
    type Prop Active Action = Bool
    getProp _ = active
    setProp _ a ca = ca { active = a }

data Actions = Actions_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Actions where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Actions :: Actions -> Actions
pattern Actions ca = ca

instance Pure Actions where
    render Actions_ {..} =
        let
            cs =
                ( "Actionss"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Actions where
    type Prop As Actions = Features -> [View] -> View
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasFeatures Actions where
    getFeatures = features
    setFeatures as ca = ca { features = as }

instance HasChildren Actions where
    getChildren = children
    setChildren cs ca = ca { children = cs }


data Author = Author_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Author where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Author :: Author -> Author
pattern Author ca = ca

instance Pure Author where
    render Author_ {..} =
        let
            cs =
                ( "author"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Author where
    type Prop As Author = Features -> [View] -> View
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasFeatures Author where
    getFeatures = features
    setFeatures as ca = ca { features = as }

instance HasChildren Author where
    getChildren = children
    setChildren cs ca = ca { children = cs }


data Avatar = Avatar_
    { as :: Features -> [View] -> View
    , features :: Features
    , src :: Txt
    } deriving (Generic)

instance Default Avatar where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Avatar :: Avatar -> Avatar
pattern Avatar ca = ca

instance Pure Avatar where
    render Avatar_ {..} =
        let
            cs =
                ( "avatar"
                )
        in
            as
                : attributes
                )
                [Img [ HTML.Src src ] []]

instance HasProp As Avatar where
    type Prop As Avatar = Features -> [View] -> View
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasFeatures Avatar where
    getFeatures = features
    setFeatures as ca = ca { features = as }


instance HasProp Src Avatar where
    type Prop Src Avatar = Txt
    getProp _ = src
    setProp _ s ca = ca { src = s }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content cc = cc

instance Pure Content where
    render Content_ {..} =
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
    setProp _ a cc = cc { as = a }

instance HasFeatures Content where
    getFeatures = features
    setFeatures as cc = cc { features = as }

instance HasChildren Content where
    getChildren = children
    setChildren cs cc = cc { children = cs }


data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , collapsed :: Bool
    , minimal :: Bool
    , size :: Txt
    , threaded :: Bool
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group cg = cg

instance Pure Group where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : size
                : collapsed # "collapsed"
                : minimal # "minimal"
                : threaded # "threaded"
                : "comments"
                )
        in
            as
                : attributes
                )
                children

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


instance HasProp Collapsed Group where
    type Prop Collapsed Group = Bool
    getProp _ = collapsed
    setProp _ c cg = cg { collapsed = c }

instance HasProp Minimal Group where
    type Prop Minimal Group = Bool
    getProp _ = minimal
    setProp _ m cg = cg { minimal = m }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ s cg = cg { size = s }

instance HasProp Threaded Group where
    type Prop Threaded Group = Bool
    getProp _ = threaded
    setProp _ t cg = cg { threaded = t }

data Metadata = Metadata_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Metadata where
    def = (G.to gdef) { as = \fs cs -> A & Features fs & Children cs }

pattern Metadata :: Metadata -> Metadata
pattern Metadata cm = cm

instance Pure Metadata where
    render Metadata_ {..} =
        let
            cs =
                ( "metadata"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Metadata where
    type Prop As Metadata = Features -> [View] -> View
    getProp _ = as
    setProp _ a cm = cm { as = a }

instance HasFeatures Metadata where
    getFeatures = features
    setFeatures as cm = cm { features = as }

instance HasChildren Metadata where
    getChildren = children
    setChildren cs cm = cm { children = cs }


data Text = Text_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Text where
    def = (G.to gdef) { as = \fs cs -> A & Features fs & Children cs }

pattern Text :: Text -> Text
pattern Text ct = ct

instance Pure Text where
    render Text_ {..} =
        let
            cs =
                ( "text"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Text where
    type Prop As Text = Features -> [View] -> View
    getProp _ = as
    setProp _ a ct = ct { as = a }

instance HasFeatures Text where
    getFeatures = features
    setFeatures as ct = ct { features = as }

instance HasChildren Text where
    getChildren = children
    setChildren cs ct = ct { children = cs }

