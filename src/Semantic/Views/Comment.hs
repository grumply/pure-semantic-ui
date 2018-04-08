module Semantic.Views.Comment where

import GHC.Generics as G
import Pure.View hiding (Action,active)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasCollapsedProp(..), pattern Collapsed
  , HasActiveProp(..), pattern Active
  , HasMinimalProp(..), pattern Minimal
  , HasSizeProp(..), pattern Size
  , HasThreadedProp(..), pattern Threaded
  , HasSrcProp(..), pattern Src
  )

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

instance HasAsProp (Comment ms) where
    type AsProp (Comment ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a c = c { as = a }

instance HasAttributesProp (Comment ms) where
    type Attribute (Comment ms) = Feature ms
    getAttributes = attributes
    setAttributes as c = c { attributes = as }

instance HasChildrenProp (Comment ms) where
    type Child (Comment ms) = View ms
    getChildren = children
    setChildren cs c = c { children = cs }

instance HasClassesProp (Comment ms) where
    getClasses = classes
    setClasses cs c = c { classes = cs }

instance HasCollapsedProp (Comment ms) where
    getCollapsed = collapsed
    setCollapsed c com = com { collapsed = c }

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

instance HasAsProp (Action ms) where
    type AsProp (Action ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ca = ca { as = a }

instance HasAttributesProp (Action ms) where
    type Attribute (Action ms) = Feature ms
    getAttributes = attributes
    setAttributes as ca = ca { attributes = as }

instance HasChildrenProp (Action ms) where
    type Child (Action ms) = View ms
    getChildren = children
    setChildren cs ca = ca { children = cs }

instance HasClassesProp (Action ms) where
    getClasses = classes
    setClasses cs ca = ca { classes = cs }

instance HasActiveProp (Action ms) where
    getActive = active
    setActive a ca = ca { active = a }

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

instance HasAsProp (Actions ms) where
    type AsProp (Actions ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ca = ca { as = a }

instance HasAttributesProp (Actions ms) where
    type Attribute (Actions ms) = Feature ms
    getAttributes = attributes
    setAttributes as ca = ca { attributes = as }

instance HasChildrenProp (Actions ms) where
    type Child (Actions ms) = View ms
    getChildren = children
    setChildren cs ca = ca { children = cs }

instance HasClassesProp (Actions ms) where
    getClasses = classes
    setClasses cs ca = ca { classes = cs }

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

instance HasAsProp (Author ms) where
    type AsProp (Author ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ca = ca { as = a }

instance HasAttributesProp (Author ms) where
    type Attribute (Author ms) = Feature ms
    getAttributes = attributes
    setAttributes as ca = ca { attributes = as }

instance HasChildrenProp (Author ms) where
    type Child (Author ms) = View ms
    getChildren = children
    setChildren cs ca = ca { children = cs }

instance HasClassesProp (Author ms) where
    getClasses = classes
    setClasses cs ca = ca { classes = cs }

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

instance HasAsProp (Avatar ms) where
    type AsProp (Avatar ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ca = ca { as = a }

instance HasAttributesProp (Avatar ms) where
    type Attribute (Avatar ms) = Feature ms
    getAttributes = attributes
    setAttributes as ca = ca { attributes = as }

instance HasClassesProp (Avatar ms) where
    getClasses = classes
    setClasses cs ca = ca { classes = cs }

instance HasSrcProp (Avatar ms) where
    getSrc = src
    setSrc s ca = ca { src = s }

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

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cc = cc { as = a }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes as cc = cc { attributes = as }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs cc = cc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs cc = cc { classes = cs }

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

instance HasAsProp (Group ms) where
    type AsProp (Group ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cg = cg { as = a }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes as cg = cg { attributes = as }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs cg = cg { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs cg = cg { classes = cs }

instance HasCollapsedProp (Group ms) where
    getCollapsed = collapsed
    setCollapsed c cg = cg { collapsed = c }

instance HasMinimalProp (Group ms) where
    getMinimal = minimal
    setMinimal m cg = cg { minimal = m }

instance HasSizeProp (Group ms) where
    getSize = size
    setSize s cg = cg { size = s }

instance HasThreadedProp (Group ms) where
    getThreaded = threaded
    setThreaded t cg = cg { threaded = t }

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

instance HasAsProp (Metadata ms) where
    type AsProp (Metadata ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cm = cm { as = a }

instance HasAttributesProp (Metadata ms) where
    type Attribute (Metadata ms) = Feature ms
    getAttributes = attributes
    setAttributes as cm = cm { attributes = as }

instance HasChildrenProp (Metadata ms) where
    type Child (Metadata ms) = View ms
    getChildren = children
    setChildren cs cm = cm { children = cs }

instance HasClassesProp (Metadata ms) where
    getClasses = classes
    setClasses cs cm = cm { classes = cs }

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

instance HasAsProp (Text ms) where
    type AsProp (Text ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ct = ct { as = a }

instance HasAttributesProp (Text ms) where
    type Attribute (Text ms) = Feature ms
    getAttributes = attributes
    setAttributes as ct = ct { attributes = as }

instance HasChildrenProp (Text ms) where
    type Child (Text ms) = View ms
    getChildren = children
    setChildren cs ct = ct { children = cs }

instance HasClassesProp (Text ms) where
    getClasses = classes
    setClasses cs ct = ct { classes = cs }
