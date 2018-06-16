module Semantic.Proxy
  ( module Properties
  , module Tools
  , Proxy(..), pattern Proxy
  )where

import Pure

import Control.Monad (join)
import Data.Foldable (traverse_)
import Data.IORef (readIORef)
import Data.Traversable (for)
import GHC.Generics (Generic)

import System.IO.Unsafe (unsafePerformIO)

import Semantic.Utils
import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern InnerRef, InnerRef(..)
  , pattern OnMount, OnMount(..)
  , pattern OnMounted, OnMounted(..)
  , pattern OnUnmounted, OnUnmounted(..)
  )

data Proxy = Proxy_
    { child :: View
    , innerRef :: Node -> IO ()
    , onMount :: IO ()
    , onMounted :: IO ()
    , onUnmounted :: IO ()
    } deriving (Generic)

instance Default Proxy

pattern Proxy :: Proxy -> Proxy
pattern Proxy a = a

instance Pure Proxy where
    view = LibraryComponentIO $ \self ->
        let
            withRef (getHost -> h) = do
                f <- innerRef <$> ask self
                traverse_ f h
        in
            def
                { construct = return ()
                , mount     = \_ ->   ask  self >>= onMount
                , mounted   =         ask  self >>= onMounted >>
                                      look self >>= withRef
                , unmounted =         ask  self >>= onUnmounted
                , updated   = \_ _ -> look self >>= withRef
                , render    = \r _ -> child r
                }

instance HasChildren Proxy where
    getChildren v = [ child v ]
    setChildren cs v = v { child = only cs }

instance HasProp InnerRef Proxy where
    type Prop InnerRef Proxy = Node -> IO ()
    getProp _ = innerRef
    setProp _ ir p = p { innerRef = ir }

instance HasProp OnMount Proxy where
    type Prop OnMount Proxy = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnMounted Proxy where
    type Prop OnMounted Proxy = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMounted = om }

instance HasProp OnUnmounted Proxy where
    type Prop OnUnmounted Proxy = IO ()
    getProp _ = onUnmounted
    setProp _ om p = p { onUnmounted = om }
