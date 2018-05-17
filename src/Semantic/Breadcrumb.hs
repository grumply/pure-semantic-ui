module Semantic.Breadcrumb
  ( module Properties
  , module Tools
  , Breadcrumb(..), pattern Breadcrumb
  , Divider(..), pattern Divider
  , Section(..), pattern Section
  ) where

import GHC.Generics as G (Generic,to)
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML hiding (Section)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Size, Size(..)
  , pattern Active, Active(..)
  , pattern Link, Link(..)
  )

import qualified Data.List as List

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Breadcrumb = Breadcrumb_
    { as       :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , size     :: Txt
    } deriving (Generic)

instance Default Breadcrumb where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Breadcrumb :: Breadcrumb -> View
pattern Breadcrumb bc = View bc

instance Pure Breadcrumb where
    view Breadcrumb_ {..} =
        let
            cs =
                [ "ui"
                , size
                , "breadcrumb"
                ]
        in
            as (features & AddClasses cs) children

instance HasProp As Breadcrumb where
    type Prop As Breadcrumb = Features -> [View] -> View
    getProp _ = as
    setProp _ a bc = bc { as = a }

instance HasFeatures Breadcrumb where
    getFeatures = features
    setFeatures fs bc = bc { features = fs }

instance HasChildren Breadcrumb where
    getChildren = children
    setChildren cs bc = bc { children = cs }

instance HasProp Size Breadcrumb where
    type Prop Size Breadcrumb = Txt
    getProp _ = size
    setProp _ sz bc = bc { size = sz }

data Divider = Divider_
    { as       :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Divider where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Divider :: Divider -> Divider
pattern Divider bcd = bcd

instance Pure Divider where
    view Divider_ {..} = as (features & Class "divider") (Prelude.null children ? [ "/" ] $ children)

instance HasProp As Divider where
    type Prop As Divider = Features -> [View] -> View
    getProp _ = as
    setProp _ a bcd = bcd { as = a }

instance HasFeatures Divider where
    getFeatures = features
    setFeatures fs bcd = bcd { features = fs }

instance HasChildren Divider where
    getChildren = children
    setChildren cs bcd = bcd { children = cs }

data Section = Section_
    { as         :: Features -> [View] -> View
    , features   :: Features
    , children   :: [View]
    , active     :: Bool
    , link       :: Bool
    } deriving (Generic)

instance Default Section where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Section :: Section -> Section
pattern Section bcs = bcs

instance Pure Section where
    view Section_ {..} =
        let
            cs =
                [ active # "active"
                , "section"
                ]
        in
            as (features & AddClasses cs) children

instance HasProp As Section where
    type Prop As Section = Features -> [View] -> View
    getProp _ = as
    setProp _ a bcs = bcs { as = a }

instance HasFeatures Section where
    getFeatures = features
    setFeatures fs bcs = bcs { features = fs }

instance HasChildren Section where
    getChildren = children
    setChildren cs bcs = bcs { children = cs }

instance HasProp Active Section where
    type Prop Active Section = Bool
    getProp _ = active
    setProp _ a bcs = bcs { active = a }

instance HasProp Link Section where
    type Prop Link Section = Bool
    getProp _ = link
    setProp _ l bcs = bcs { link = l }
