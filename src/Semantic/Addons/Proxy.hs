module Semantic.Addons.Proxy
  ( module Properties
  , module Tools
  , Proxy(..), pattern Proxy
  )where

import GHC.Generics as G
import Pure.View hiding (children,Proxy)
import Pure.Lifted (Node)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasChildrenProp(..), pattern Children
  , HasInnerRefProp(..), pattern InnerRef
  )

data Proxy ms = Proxy_
    { children :: [View ms]
    , innerRef :: Node -> IO ()
    } deriving (Generic)

instance Default (Proxy ms)

pattern Proxy :: Proxy ms -> View ms
pattern Proxy r = View r

instance Pure Proxy ms where
    render r =
        Component "Semantic.Addons.Proxy" r $ \self -> def
            { construct = return ()
            , mounted = getView self >>= traverse_ (innerRef r) . getHost
            , renderer = \ref _ -> only (children ref)
            }

instance HasChildrenProp (Proxy ms) where
    type Child (Proxy ms) = View ms
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasInnerRefProp (Proxy ms) where
    type InnerRefProp (Proxy ms) = Node -> IO ()
    getInnerRef = innerRef
    setInnerRef ir p = p { innerRef = ir }
