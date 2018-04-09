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
import Pure.View hiding (text,Event,Meta,Label,Content)

import Semantic.Utils

import Semantic.Image (Image(..))

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Size, Size(..)
  , pattern IsText, IsText(..)
  )

import Data.Function as Tool ((&))
import Pure.Data.Default as Tools

data Feed ms = Feed_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Feed ms) where
    def = (G.to gdef) { as = Div }

pattern Feed :: Feed ms -> View ms
pattern Feed f = View f

instance Pure Feed ms where
    render Feed_ {..} =
        let
            cs =
                ( "ui"
                : size
                : "feed"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Feed ms) where
    type Prop As (Feed ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a f = f { as = a }

instance HasProp Attributes (Feed ms) where
    type Prop Attributes (Feed ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as f = f { attributes = as }

instance HasProp Children (Feed ms) where
    type Prop Children (Feed ms) = [View ms]
    getProp _ = children
    setProp _ cs f = f { children = cs }

instance HasProp Classes (Feed ms) where
    type Prop Classes (Feed ms) = [Txt]
    getProp _ = classes
    setProp _ cs f = f { classes = cs }

instance HasProp Size (Feed ms) where
    type Prop Size (Feed ms) = Txt
    getProp _ = size
    setProp _ s f = f { size = s }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content fc = View fc

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
    setProp _ a fc = fc { as = a }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fc = fc { attributes = as }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs fc = fc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs fc = fc { classes = cs }

data Date ms = Date_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Date ms) where
    def = (G.to gdef) { as = Div }

pattern Date :: Date ms -> View ms
pattern Date fd = View fd

instance Pure Date ms where
    render Date_ {..} =
        let
            cs =
                ( "date"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Date ms) where
    type Prop As (Date ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a fd = fd { as = a }

instance HasProp Attributes (Date ms) where
    type Prop Attributes (Date ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fd = fd { attributes = as }

instance HasProp Children (Date ms) where
    type Prop Children (Date ms) = [View ms]
    getProp _ = children
    setProp _ cs fd = fd { children = cs }

instance HasProp Classes (Date ms) where
    type Prop Classes (Date ms) = [Txt]
    getProp _ = classes
    setProp _ cs fd = fd { classes = cs }

data Event ms = Event_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Event ms) where
    def = (G.to gdef) { as = Div }

pattern Event :: Event ms -> View ms
pattern Event fe = View fe

instance Pure Event ms where
    render Event_ {..} =
        let
            cs =
                ( "event"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Event ms) where
    type Prop As (Event ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a fe = fe { as = a }

instance HasProp Attributes (Event ms) where
    type Prop Attributes (Event ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fe = fe { attributes = as }

instance HasProp Children (Event ms) where
    type Prop Children (Event ms) = [View ms]
    getProp _ = children
    setProp _ cs fe = fe { children = cs }

instance HasProp Classes (Event ms) where
    type Prop Classes (Event ms) = [Txt]
    getProp _ = classes
    setProp _ cs fe = fe { classes = cs }

data Extra ms = Extra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (Extra ms) where
    def = (G.to gdef) { as = Div }

pattern Extra :: Extra ms -> View ms
pattern Extra fe = View fe

instance Pure Extra ms where
    render Extra_ {..} =
        let
            image = foldPures (\(Image_ {}) -> const True) False children

            cs =
                ( image # "images"
                : text # "text"
                : "extra"
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
    setProp _ a fe = fe { as = a }

instance HasProp Attributes (Extra ms) where
    type Prop Attributes (Extra ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fe = fe { attributes = as }

instance HasProp Children (Extra ms) where
    type Prop Children (Extra ms) = [View ms]
    getProp _ = children
    setProp _ cs fe = fe { children = cs }

instance HasProp Classes (Extra ms) where
    type Prop Classes (Extra ms) = [Txt]
    getProp _ = classes
    setProp _ cs fe = fe { classes = cs }

instance HasProp IsText (Extra ms) where
    type Prop IsText (Extra ms) = Bool
    getProp _ = text
    setProp _ it fe = fe { text = it }

data Label ms = Label_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Label ms) where
    def = (G.to gdef) { as = Div }

pattern Label :: Label ms -> View ms
pattern Label fl = View fl

instance Pure Label ms where
    render Label_ {..} =
        let
            cs =
                ( "label"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Label ms) where
    type Prop As (Label ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a fl = fl { as = a }

instance HasProp Attributes (Label ms) where
    type Prop Attributes (Label ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fl = fl { attributes = as }

instance HasProp Children (Label ms) where
    type Prop Children (Label ms) = [View ms]
    getProp _ = children
    setProp _ cs fl = fl { children = cs }

instance HasProp Classes (Label ms) where
    type Prop Classes (Label ms) = [Txt]
    getProp _ = classes
    setProp _ cs fl = fl { classes = cs }

data Like ms = Like_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Like ms) where
    def = (G.to gdef) { as = Div }

pattern Like :: Like ms -> View ms
pattern Like fl = View fl

instance Pure Like ms where
    render Like_ {..} =
        let
            cs =
                ( "like"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Like ms) where
    type Prop As (Like ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a fl = fl { as = a }

instance HasProp Attributes (Like ms) where
    type Prop Attributes (Like ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fl = fl { attributes = as }

instance HasProp Children (Like ms) where
    type Prop Children (Like ms) = [View ms]
    getProp _ = children
    setProp _ cs fl = fl { children = cs }

instance HasProp Classes (Like ms) where
    type Prop Classes (Like ms) = [Txt]
    getProp _ = classes
    setProp _ cs fl = fl { classes = cs }

data Meta ms = Meta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Meta ms) where
    def = (G.to gdef) { as = Div }

pattern Meta :: Meta ms -> View ms
pattern Meta fm = View fm

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
    setProp _ a fm = fm { as = a }

instance HasProp Attributes (Meta ms) where
    type Prop Attributes (Meta ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fm = fm { attributes = as }

instance HasProp Children (Meta ms) where
    type Prop Children (Meta ms) = [View ms]
    getProp _ = children
    setProp _ cs fm = fm { children = cs }

instance HasProp Classes (Meta ms) where
    type Prop Classes (Meta ms) = [Txt]
    getProp _ = classes
    setProp _ cs fm = fm { classes = cs }

data Summary ms = Summary_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Summary ms) where
    def = (G.to gdef) { as = Div }

pattern Summary :: Summary ms -> View ms
pattern Summary fs = View fs

instance Pure Summary ms where
    render Summary_ {..} =
        let
            cs =
                ( "summary"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Summary ms) where
    type Prop As (Summary ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a fs = fs { as = a }

instance HasProp Attributes (Summary ms) where
    type Prop Attributes (Summary ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fs = fs { attributes = as }

instance HasProp Children (Summary ms) where
    type Prop Children (Summary ms) = [View ms]
    getProp _ = children
    setProp _ cs fs = fs { children = cs }

instance HasProp Classes (Summary ms) where
    type Prop Classes (Summary ms) = [Txt]
    getProp _ = classes
    setProp _ cs fs = fs { classes = cs }

data User ms = User_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (User ms) where
    def = (G.to gdef) { as = A }

pattern User :: User ms -> View ms
pattern User fu = View fu

instance Pure User ms where
    render User_ {..} =
        let
            cs =
                ( "user"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (User ms) where
    type Prop As (User ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a fu = fu { as = a }

instance HasProp Attributes (User ms) where
    type Prop Attributes (User ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fu = fu { attributes = as }

instance HasProp Children (User ms) where
    type Prop Children (User ms) = [View ms]
    getProp _ = children
    setProp _ cs fu = fu { children = cs }

instance HasProp Classes (User ms) where
    type Prop Classes (User ms) = [Txt]
    getProp _ = classes
    setProp _ cs fu = fu { classes = cs }
