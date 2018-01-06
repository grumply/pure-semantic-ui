module Semantic.Modules.Sidebar.SidebarPusher where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

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
