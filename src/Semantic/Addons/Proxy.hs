module Semantic.Addons.Proxy where

import GHC.Generics as G
import Pure.View hiding (children,Proxy)
import Pure.Lifted (Node)

import Semantic.Utils

data Proxy ms = Proxy_
    { children :: [View ms]
    , innerRef :: Node -> IO ()
    } deriving (Generic)

instance Default (Proxy ms)

pattern Proxy :: Typeable ms => Proxy ms -> View ms
pattern Proxy r = View r

instance Typeable ms => Pure Proxy ms where
    render r =
        Component "Semantic.Addons.Proxy" r $ \self -> def
            { construct = return ()
            , mounted = getView self >>= traverse_ (innerRef r) . getHost
            , renderer = \ref _ -> only (children ref)
            }
