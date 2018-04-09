module Semantic.Collections.Message
  ( module Properties
  , module Tools
  , Message(..), pattern Message
  , Content(..), pattern Content
  , Header(..), pattern Header
  , Item(..), pattern Item
  , List(..), pattern List
  ) where

import GHC.Generics as G
import Pure.View hiding (color,hidden,visible,Name,Content,Header)

import Semantic.Utils

import Semantic.Elements.Icon

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Name, Name(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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

data Message ms = Message_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , color :: Txt
    , compact :: Bool
    , error :: Bool
    , floating :: Bool
    , hidden :: Bool
    , info :: Bool
    , negative :: Bool
    , onDismiss :: Ef ms IO ()
    , positive :: Bool
    , size :: Txt
    , success :: Bool
    , visible :: Bool
    , warning :: Bool
    } deriving (Generic)

instance Default (Message ms) where
    def = (G.to gdef) { as = Div }

pattern Message :: Message ms -> View ms
pattern Message m = View m

instance Pure Message ms where
    render Message_ {..} =
        let
            icon = foldPures (\(Icon_ {}) -> const True) False children

            dismissIcon = onDismiss # (Icon $ def & Name "close" & Attributes [ On "click" def (\_ -> return $ Just onDismiss) ])

            cs =
                ( "ui"
                : color
                : size
                : compact # "compact"
                : error # "error"
                : floating # "floating"
                : hidden # "hidden"
                : icon # "icon"
                : info # "info"
                : negative # "negative"
                : positive # "positive"
                : success # "success"
                : visible # "visible"
                : warning # "warning"
                : may (<>> "attached") attached
                : "message"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                ( dismissIcon : children )

instance HasProp As (Message ms) where
    type Prop As (Message ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a m = m { as = a }

instance HasProp Attributes (Message ms) where
    type Prop Attributes (Message ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as m = m { attributes = as }

instance HasProp Children (Message ms) where
    type Prop Children (Message ms) = [View ms]
    getProp _ = children
    setProp _ cs m = m { children = cs }

instance HasProp Classes (Message ms) where
    type Prop Classes (Message ms) = [Txt]
    getProp _ = classes
    setProp _ cs m = m { classes = cs }

instance HasProp Attached (Message ms) where
    type Prop Attached (Message ms) = Maybe Txt
    getProp _ = attached
    setProp _ a m = m { attached = a }

instance HasProp Color (Message ms) where
    type Prop Color (Message ms) = Txt
    getProp _ = color
    setProp _ c m = m { color = c }

instance HasProp Compact (Message ms) where
    type Prop Compact (Message ms) = Bool
    getProp _ = compact
    setProp _ c m = m { compact = c }

instance HasProp Error (Message ms) where
    type Prop Error (Message ms) = Bool
    getProp _ = error
    setProp _ e m = m { error = e }

instance HasProp Floating (Message ms) where
    type Prop Floating (Message ms) = Bool
    getProp _ = floating
    setProp _ f m = m { floating = f }

instance HasProp Hidden (Message ms) where
    type Prop Hidden (Message ms) = Bool
    getProp _ = hidden
    setProp _ h m = m { hidden = h }

instance HasProp Info (Message ms) where
    type Prop Info (Message ms) = Bool
    getProp _ = info
    setProp _ i m = m { info = i }

instance HasProp Negative (Message ms) where
    type Prop Negative (Message ms) = Bool
    getProp _ = negative
    setProp _ n m = m { negative = n }

instance HasProp OnDismiss (Message ms) where
    type Prop OnDismiss (Message ms) = Ef ms IO ()
    getProp _ = onDismiss
    setProp _ od m = m { onDismiss = od }

instance HasProp Positive (Message ms) where
    type Prop Positive (Message ms) = Bool
    getProp _ = positive
    setProp _ p m = m { positive = p }

instance HasProp Size (Message ms) where
    type Prop Size (Message ms) = Txt
    getProp _ = size
    setProp _ s m = m { size = s }

instance HasProp Success (Message ms) where
    type Prop Success (Message ms) = Bool
    getProp _ = success
    setProp _ s m = m { success = s }

instance HasProp Visible (Message ms) where
    type Prop Visible (Message ms) = Bool
    getProp _ = visible
    setProp _ v m = m { visible = v }

instance HasProp Warning (Message ms) where
    type Prop Warning (Message ms) = Bool
    getProp _ = warning
    setProp _ w m = m { warning = w }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content mc = View mc

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
    setProp _ a mc = mc { as = a }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as mc = mc { attributes = as }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs mc = mc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs mc = mc { classes = cs }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header mh = View mh

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
    setProp _ a mh = mh { as = a }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as mh = mh { attributes = as }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs mh = mh { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs mh = mh { classes = cs }

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Li }

pattern Item :: Item ms -> View ms
pattern Item mi = View mi

instance Pure Item ms where
    render Item_ {..} =
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

instance HasProp As (Item ms) where
    type Prop As (Item ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a mi = mi { as = a }

instance HasProp Attributes (Item ms) where
    type Prop Attributes (Item ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as mi = mi { attributes = as }

instance HasProp Children (Item ms) where
    type Prop Children (Item ms) = [View ms]
    getProp _ = children
    setProp _ cs mi = mi { children = cs }

instance HasProp Classes (Item ms) where
    type Prop Classes (Item ms) = [Txt]
    getProp _ = classes
    setProp _ cs mi = mi { classes = cs }

data List ms = List_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (List ms) where
    def = (G.to gdef) { as = Ul }

pattern List :: List ms -> View ms
pattern List ml = View ml

instance Pure List ms where
    render List_ {..} =
        let
            cs =
                ( "list"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (List ms) where
    type Prop As (List ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ml = ml { as = a }

instance HasProp Attributes (List ms) where
    type Prop Attributes (List ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ml = ml { attributes = as }

instance HasProp Children (List ms) where
    type Prop Children (List ms) = [View ms]
    getProp _ = children
    setProp _ cs ml = ml { children = cs }

instance HasProp Classes (List ms) where
    type Prop Classes (List ms) = [Txt]
    getProp _ = classes
    setProp _ cs ml = ml { classes = cs }

