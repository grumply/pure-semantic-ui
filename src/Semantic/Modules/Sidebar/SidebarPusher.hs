module Semantic.Modules.Sidebar.SidebarPusher where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Dimmed

data SidebarPusher ms = SidebarPusher_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , dimmed :: Bool
    } deriving (Generic)

instance Default (SidebarPusher ms) where
    def = (G.to gdef) { as = Div }

pattern SidebarPusher :: Typeable ms => SidebarPusher ms -> View ms
pattern SidebarPusher sp = View sp

instance Typeable ms => Pure SidebarPusher ms where
    render SidebarPusher_ {..} =
        let
            cs = 
                ( "pusher"
                : dimmed # "dimmed"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (SidebarPusher ms) where
    type AsProp (SidebarPusher ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sp = sp { as = a }

instance HasAttributesProp (SidebarPusher ms) where
    type Attribute (SidebarPusher ms) = Feature ms
    getAttributes = attributes
    setAttributes as sp = sp { attributes = as }

instance HasChildrenProp (SidebarPusher ms) where
    type Child (SidebarPusher ms) = View ms
    getChildren = children
    setChildren cs sp = sp { children = cs }

instance HasClassesProp (SidebarPusher ms) where
    getClasses = classes
    setClasses cs sp = sp { classes = cs }

instance HasDimmedProp (SidebarPusher ms) where
    getDimmed = dimmed
    setDimmed d sp = sp { dimmed = d }
