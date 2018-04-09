module Semantic.Breadcrumb
  ( module Properties
  , module Tools
  , Breadcrumb(..), pattern Breadcrumb
  , Divider(..), pattern Divider
  , Section(..), pattern Section
  ) where

import GHC.Generics as G
import Pure.View hiding ((!),Ref,name,active,onClick,Section)

import Semantic.Utils

import Semantic.Properties ((!))

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Size, Size(..)
  , pattern Active, Active(..)
  , pattern Ref, Ref(..)
  , pattern Link, Link(..)
  , pattern OnClick, OnClick(..)
  )

import Semantic.Properties
  ( pattern Ref, Ref(..)
  , pattern Active, Active(..)
  )

import qualified Data.List as List

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Breadcrumb ms = Breadcrumb_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Breadcrumb ms) where
    def = (G.to gdef) { as = Div }

pattern Breadcrumb :: Breadcrumb ms -> View ms
pattern Breadcrumb bc = View bc

instance Pure Breadcrumb ms where
    render Breadcrumb_ {..} =
        let
            cs =
                ( "ui"
                : size
                : "breadcrumb"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Breadcrumb ms) where
    type Prop As (Breadcrumb ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a bc = bc { as = a }

instance HasProp Attributes (Breadcrumb ms) where
    type Prop Attributes (Breadcrumb ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as bc = bc { attributes = as }

instance HasProp Children (Breadcrumb ms) where
    type Prop Children (Breadcrumb ms) = [View ms]
    getProp _ = children
    setProp _ cs bc = bc { children = cs }

instance HasProp Classes (Breadcrumb ms) where
    type Prop Classes (Breadcrumb ms) = [Txt]
    getProp _ = classes
    setProp _ cs bc = bc { classes = cs }

instance HasProp Size (Breadcrumb ms) where
    type Prop Size (Breadcrumb ms) = Txt
    getProp _ = size
    setProp _ sz bc = bc { size = sz }

data Divider ms = Divider_
    { as         :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children   :: [View ms]
    , classes    :: [Txt]
    } deriving (Generic)

instance Default (Divider ms) where
    def = (G.to gdef) { as = Div }

pattern Divider :: Divider ms -> View ms
pattern Divider bcd = View bcd

instance Pure Divider ms where
    render Divider_ {..} =
        let
            cs =
                ( "divider"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                (children ? children $ "/")

instance HasProp As (Divider ms) where
    type Prop As (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a bcd = bcd { as = a }

instance HasProp Attributes (Divider ms) where
    type Prop Attributes (Divider ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as bcd = bcd { attributes = as }

instance HasProp Children (Divider ms) where
    type Prop Children (Divider ms) = [View ms]
    getProp _ = children
    setProp _ cs bcd = bcd { children = cs }

instance HasProp Classes (Divider ms) where
    type Prop Classes (Divider ms) = [Txt]
    getProp _ = classes
    setProp _ cs bcd = bcd { classes = cs }

data Section ms = Section_
    { as         :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children   :: [View ms]
    , classes    :: [Txt]
    , active     :: Bool
    , ref        :: Feature ms
    , link       :: Bool
    , onClick    :: Ef ms IO ()
    } deriving (Generic)

instance Default (Section ms) where
    def = (G.to gdef) { as = Div }

pattern Section :: Section ms -> View ms
pattern Section bcs = View bcs

instance Pure Section ms where
    render Section_ {..} =
        let
            e = link ? A $ ref ? A $ as
            cs =
                ( active # "active"
                : "section"
                : classes
                )
        in
            e
                ( mergeClasses $ ClassList cs
                : ref
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasProp As (Section ms) where
    type Prop As (Section ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a bcs = bcs { as = a }

instance HasProp Attributes (Section ms) where
    type Prop Attributes (Section ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as bcs = bcs { attributes = as }

instance HasProp Children (Section ms) where
    type Prop Children (Section ms) = [View ms]
    getProp _ = children
    setProp _ cs bcs = bcs { children = cs }

instance HasProp Classes (Section ms) where
    type Prop Classes (Section ms) = [Txt]
    getProp _ = classes
    setProp _ cs bcs = bcs { classes = cs }

instance HasProp Active (Section ms) where
    type Prop Active (Section ms) = Bool
    getProp _ = active
    setProp _ a bcs = bcs { active = a }

instance HasProp Ref (Section ms) where
    type Prop Ref (Section ms) = Feature ms
    getProp _ = ref
    setProp _ r bcs = bcs { ref = r }

instance HasProp Link (Section ms) where
    type Prop Link (Section ms) = Bool
    getProp _ = link
    setProp _ l bcs = bcs { link = l }

instance HasProp OnClick (Section ms) where
    type Prop OnClick (Section ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc bcs = bcs { onClick = oc }
