module Semantic.Collections.Breadcrumb
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

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasSizeProp(..), pattern Size
  , HasActiveProp(..), pattern Active
  , HasRefProp(..), pattern Ref
  , HasLinkProp(..), pattern Link
  , HasOnClickProp(..), pattern OnClick
  )

import Semantic.Properties (HasRefProp(..),pattern Ref,HasActiveProp(..),pattern Active)

import qualified Data.List as List
import Data.Function ((&))

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

instance HasAsProp (Breadcrumb ms) where
    type AsProp (Breadcrumb ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a bc = bc { as = a }

instance HasAttributesProp (Breadcrumb ms) where
    type Attribute (Breadcrumb ms) = Feature ms
    getAttributes = attributes
    setAttributes as bc = bc { attributes = as }

instance HasChildrenProp (Breadcrumb ms) where
    type Child (Breadcrumb ms) = View ms
    getChildren = children
    setChildren cs bc = bc { children = cs }

instance HasClassesProp (Breadcrumb ms) where
    getClasses = classes
    setClasses cs bc = bc { classes = cs }

instance HasSizeProp (Breadcrumb ms) where
    getSize = size
    setSize sz bc = bc { size = sz }

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

instance HasAsProp (Divider ms) where
    type AsProp (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a bcd = bcd { as = a }

instance HasAttributesProp (Divider ms) where
    type Attribute (Divider ms) = Feature ms
    getAttributes = attributes
    setAttributes as bcd = bcd { attributes = as }

instance HasChildrenProp (Divider ms) where
    type Child (Divider ms) = View ms
    getChildren = children
    setChildren cs bcd = bcd { children = cs }

instance HasClassesProp (Divider ms) where
    getClasses = classes
    setClasses cs bcd = bcd { classes = cs }

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

instance HasAsProp (Section ms) where
    type AsProp (Section ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a bcs = bcs { as = a }

instance HasAttributesProp (Section ms) where
    type Attribute (Section ms) = Feature ms
    getAttributes = attributes
    setAttributes as bcs = bcs { attributes = as }

instance HasChildrenProp (Section ms) where
    type Child (Section ms) = View ms
    getChildren = children
    setChildren cs bcs = bcs { children = cs }

instance HasClassesProp (Section ms) where
    getClasses = classes
    setClasses cs bcs = bcs { classes = cs }

instance HasActiveProp (Section ms) where
    getActive = active
    setActive a bcs = bcs { active = a }

instance HasRefProp (Section ms) where
    type RefProp (Section ms) = Feature ms
    getRef = ref
    setRef r bcs = bcs { ref = r }

instance HasLinkProp (Section ms) where
    getLink = link
    setLink l bcs = bcs { link = l }

instance HasOnClickProp (Section ms) where
    type OnClickProp (Section ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc bcs = bcs { onClick = oc }
