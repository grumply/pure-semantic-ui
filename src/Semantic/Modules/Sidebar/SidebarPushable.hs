module Semantic.Modules.Sidebar.SidebarPushable where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

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

