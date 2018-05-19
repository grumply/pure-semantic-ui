module Semantic.Proxy
  ( module Properties
  , module Tools
  , Proxy(..), pattern Proxy
  )where

import Pure.Data.Lifted (Node,IsNode(..))
import Pure.Data.View hiding (children,getHost)
import Pure.Data.Default
import Pure.Data.View.Patterns

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
  , pattern OnUnmount, OnUnmount(..)
  , pattern OnUnmounted, OnUnmounted(..)
  )

import Pure.Data.Default as Tools

data Proxy = Proxy_
    { child :: View
    , innerRef :: Node -> IO ()
    , onMount :: IO ()
    , onMounted :: IO ()
    , onUnmount :: IO ()
    , onUnmounted :: IO ()
    } deriving (Generic)

instance Default Proxy

pattern Proxy :: Proxy -> Proxy
pattern Proxy a = a

instance Pure Proxy where
    view = LibraryComponentIO $ \self ->
        let
            withRef (getHost -> h) = do
                f <- innerRef <$> getProps self
                traverse_ f h
        in
            def
                { construct = return ()
                , mount     = \_ -> do
                    p <- getProps self
                    onMount p
                , mounted   = do
                    p <- getProps self
                    onMounted p
                    getView self >>= withRef
                , unmount   = do
                    p <- getProps self
                    onUnmount p
                , unmounted = do
                    p <- getProps self
                    onUnmounted p
                , updated   = \_ _ -> withRef
                , render    = \ref _ -> child ref
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

instance HasProp OnUnmount Proxy where
    type Prop OnUnmount Proxy = IO ()
    getProp _ = onUnmount
    setProp _ om p = p { onUnmount = om }

instance HasProp OnUnmounted Proxy where
    type Prop OnUnmounted Proxy = IO ()
    getProp _ = onUnmounted
    setProp _ om p = p { onUnmounted = om }

-- TODO: expose this in pure-dom
getHost :: View -> Maybe Node
getHost ComponentView {..} = join $ for record (getHost . unsafePerformIO . readIORef . crView)
getHost TextView  {..} = fmap toNode textHost
getHost SomeView {}    = Nothing
getHost x              = fmap toNode $ elementHost x
