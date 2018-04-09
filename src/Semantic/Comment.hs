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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Collapsed, Collapsed(..)
  , pattern Active, Active(..)
  , pattern Minimal, Minimal(..)
  , pattern Size, Size(..)
  , pattern Threaded, Threaded(..)
  , pattern Src, Src(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Comment ms = Comment_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , collapsed :: Bool
    } deriving (Generic)

instance Default (Comment ms) where
    def = (G.to gdef) { as = Div }

pattern Comment :: Comment ms -> View ms
pattern Comment a = View a

instance Pure Comment ms where
    render Comment_ {..} =
        let
            cs =
                ( collapsed # "collapsed"
                : "comment"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Comment ms) where
    type Prop As (Comment ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a c = c { as = a }

instance HasProp Attributes (Comment ms) where
    type Prop Attributes (Comment ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as c = c { attributes = as }

instance HasProp Children (Comment ms) where
    type Prop Children (Comment ms) = [View ms]
    getProp _ = children
    setProp _ cs c = c { children = cs }

instance HasProp Classes (Comment ms) where
    type Prop Classes (Comment ms) = [Txt]
    getProp _ = classes
    setProp _ cs c = c { classes = cs }

instance HasProp Collapsed (Comment ms) where
    type Prop Collapsed (Comment ms) = Bool
    getProp _ = collapsed
    setProp _ c com = com { collapsed = c }

data Action ms = Action_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    } deriving (Generic)

instance Default (Action ms) where
    def = (G.to gdef) { as = A }

pattern Action :: Action ms -> View ms
pattern Action ca = View ca

instance Pure Action ms where
    render Action_ {..} =
        let
            cs =
                ( active # "active"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Action ms) where
    type Prop As (Action ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasProp Attributes (Action ms) where
    type Prop Attributes (Action ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ca = ca { attributes = as }

instance HasProp Children (Action ms) where
    type Prop Children (Action ms) = [View ms]
    getProp _ = children
    setProp _ cs ca = ca { children = cs }

instance HasProp Classes (Action ms) where
    type Prop Classes (Action ms) = [Txt]
    getProp _ = classes
    setProp _ cs ca = ca { classes = cs }

instance HasProp Active (Action ms) where
    type Prop Active (Action ms) = Bool
    getProp _ = active
    setProp _ a ca = ca { active = a }

data Actions ms = Actions_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Actions ms) where
    def = (G.to gdef) { as = Div }

pattern Actions :: Actions ms -> View ms
pattern Actions ca = View ca

instance Pure Actions ms where
    render Actions_ {..} =
        let
            cs =
                ( "Actionss"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Actions ms) where
    type Prop As (Actions ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasProp Attributes (Actions ms) where
    type Prop Attributes (Actions ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ca = ca { attributes = as }

instance HasProp Children (Actions ms) where
    type Prop Children (Actions ms) = [View ms]
    getProp _ = children
    setProp _ cs ca = ca { children = cs }

instance HasProp Classes (Actions ms) where
    type Prop Classes (Actions ms) = [Txt]
    getProp _ = classes
    setProp _ cs ca = ca { classes = cs }

data Author ms = Author_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Author ms) where
    def = (G.to gdef) { as = Div }

pattern Author :: Author ms -> View ms
pattern Author ca = View ca

instance Pure Author ms where
    render Author_ {..} =
        let
            cs =
                ( "author"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Author ms) where
    type Prop As (Author ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasProp Attributes (Author ms) where
    type Prop Attributes (Author ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ca = ca { attributes = as }

instance HasProp Children (Author ms) where
    type Prop Children (Author ms) = [View ms]
    getProp _ = children
    setProp _ cs ca = ca { children = cs }

instance HasProp Classes (Author ms) where
    type Prop Classes (Author ms) = [Txt]
    getProp _ = classes
    setProp _ cs ca = ca { classes = cs }

data Avatar ms = Avatar_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , src :: Txt
    } deriving (Generic)

instance Default (Avatar ms) where
    def = (G.to gdef) { as = Div }

pattern Avatar :: Avatar ms -> View ms
pattern Avatar ca = View ca

instance Pure Avatar ms where
    render Avatar_ {..} =
        let
            cs =
                ( "avatar"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                [Img [ HTML.Src src ] []]

instance HasProp As (Avatar ms) where
    type Prop As (Avatar ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ca = ca { as = a }

instance HasProp Attributes (Avatar ms) where
    type Prop Attributes (Avatar ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ca = ca { attributes = as }

instance HasProp Classes (Avatar ms) where
    type Prop Classes (Avatar ms) = [Txt]
    getProp _ = classes
    setProp _ cs ca = ca { classes = cs }

instance HasProp Src (Avatar ms) where
    type Prop Src (Avatar ms) = Txt
    getProp _ = src
    setProp _ s ca = ca { src = s }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content cc = View cc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( "content"
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

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , collapsed :: Bool
    , minimal :: Bool
    , size :: Txt
    , threaded :: Bool
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
                : size
                : collapsed # "collapsed"
                : minimal # "minimal"
                : threaded # "threaded"
                : "comments"
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

instance HasProp Collapsed (Group ms) where
    type Prop Collapsed (Group ms) = Bool
    getProp _ = collapsed
    setProp _ c cg = cg { collapsed = c }

instance HasProp Minimal (Group ms) where
    type Prop Minimal (Group ms) = Bool
    getProp _ = minimal
    setProp _ m cg = cg { minimal = m }

instance HasProp Size (Group ms) where
    type Prop Size (Group ms) = Txt
    getProp _ = size
    setProp _ s cg = cg { size = s }

instance HasProp Threaded (Group ms) where
    type Prop Threaded (Group ms) = Bool
    getProp _ = threaded
    setProp _ t cg = cg { threaded = t }

data Metadata ms = Metadata_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Metadata ms) where
    def = (G.to gdef) { as = A }

pattern Metadata :: Metadata ms -> View ms
pattern Metadata cm = View cm

instance Pure Metadata ms where
    render Metadata_ {..} =
        let
            cs =
                ( "metadata"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Metadata ms) where
    type Prop As (Metadata ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a cm = cm { as = a }

instance HasProp Attributes (Metadata ms) where
    type Prop Attributes (Metadata ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as cm = cm { attributes = as }

instance HasProp Children (Metadata ms) where
    type Prop Children (Metadata ms) = [View ms]
    getProp _ = children
    setProp _ cs cm = cm { children = cs }

instance HasProp Classes (Metadata ms) where
    type Prop Classes (Metadata ms) = [Txt]
    getProp _ = classes
    setProp _ cs cm = cm { classes = cs }

data Text ms = Text_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Text ms) where
    def = (G.to gdef) { as = A }

pattern Text :: Text ms -> View ms
pattern Text ct = View ct

instance Pure Text ms where
    render Text_ {..} =
        let
            cs =
                ( "text"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Text ms) where
    type Prop As (Text ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ct = ct { as = a }

instance HasProp Attributes (Text ms) where
    type Prop Attributes (Text ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ct = ct { attributes = as }

instance HasProp Children (Text ms) where
    type Prop Children (Text ms) = [View ms]
    getProp _ = children
    setProp _ cs ct = ct { children = cs }

instance HasProp Classes (Text ms) where
    type Prop Classes (Text ms) = [Txt]
    getProp _ = classes
    setProp _ cs ct = ct { classes = cs }
