module Semantic.Addons.Proxy
  ( module Properties
  , module Tools
  , Proxy(..), pattern Proxy
  )where

import GHC.Generics as G
import Pure.View hiding (children,Proxy)
import Pure.Lifted (Node)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Children, Children(..)
  , pattern InnerRef, InnerRef(..)
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

instance HasProp Children (Proxy ms) where
    type Prop Children (Proxy ms) = [View ms]
    getProp _ = children
    setProp _ cs p = p { children = cs }

instance HasProp InnerRef (Proxy ms) where
    type Prop InnerRef (Proxy ms) = Node -> IO ()
    getProp _ = innerRef
    setProp _ ir p = p { innerRef = ir }
