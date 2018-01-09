module Semantic.Properties.Renderer where

import Semantic.Properties.Utils

class HasRendererProp a where
    type RendererProp a
    getRenderer :: a -> RendererProp a
    setRenderer :: RendererProp a -> a -> a

pattern Renderer :: HasRendererProp a => RendererProp a -> a -> a
pattern Renderer r a <- (getRenderer &&& id -> (r,a)) where
    Renderer r a = setRenderer r a