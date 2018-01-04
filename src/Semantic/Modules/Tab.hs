module Semantic.Modules.Tab (module Semantic.Modules.Tab, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data Tab ms = Tab_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Tab ms) where
    def = (G.to gdef) { as = Div }

pattern Tab :: Typeable ms => Tab ms -> View ms
pattern Tab t = View t

instance Typeable ms => Pure Tab ms where
    render Tab_ {..} = as ( ClassList classes : attributes ) children
