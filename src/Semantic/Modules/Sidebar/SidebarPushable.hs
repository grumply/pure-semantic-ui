module Semantic.Modules.Sidebar.SidebarPushable where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data SidebarPushable ms = SidebarPushable_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (SidebarPushable ms) where
    def = (G.to gdef) { as = Div }

pattern SidebarPushable :: Typeable ms => SidebarPushable ms -> View ms
pattern SidebarPushable sp = View sp

instance Typeable ms => Pure SidebarPushable ms where
    render SidebarPushable_ {..} =
        let
            cs = 
                ( "pushable"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (SidebarPushable ms) where
    type AsProp (SidebarPushable ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sp = sp { as = a }

instance HasAttributesProp (SidebarPushable ms) where
    type Attribute (SidebarPushable ms) = Feature ms
    getAttributes = attributes
    setAttributes as sp = sp { attributes = as }

instance HasChildrenProp (SidebarPushable ms) where
    type Child (SidebarPushable ms) = View ms
    getChildren = children
    setChildren cs sp = sp { children = cs }

instance HasClassesProp (SidebarPushable ms) where
    getClasses = classes
    setClasses cs sp = sp { classes = cs }
