module Semantic.Proxy
  ( module Properties
  , module Tools
  , Proxy(..), pattern Proxy
  )where

import Pure.DOM
import Pure.Data.View hiding (children)
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
  )

import Pure.Data.Default as Tools

data Proxy = Proxy_
    { child :: View
    , innerRef :: Node -> IO ()
    } deriving (Generic)

instance Default Proxy

-- Proxy def <| ...
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
                , mounted   = getView self >>= withRef
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

-- TODO: expose this in pure-dom
getHost :: View -> Maybe Node
getHost ComponentView {..} = join $ for record (getHost . unsafePerformIO . readIORef . crView)
getHost TextView  {..} = fmap toNode textHost
getHost SomeView {}    = Nothing
getHost x              = fmap toNode $ elementHost x
