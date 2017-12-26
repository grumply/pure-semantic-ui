module Semantic.Elements.List.ListIcon where

import GHC.Generics as G
import Pure.View hiding (verticalAlign)

import Semantic.Utils

import Semantic.Elements.Icon as Icon hiding (IconGroup(..))

data ListIcon ms = ListIcon_ { icon :: Icon ms, verticalAlign :: Txt }
    deriving (Generic)

instance Default (ListIcon ms)

pattern ListIcon :: Typeable ms => ListIcon ms -> View ms
pattern ListIcon li = View li

instance Typeable ms => Pure ListIcon ms where
    render ListIcon_ {..} = 
        let va = verticalAlign # (verticalAlign <>> "aligned")
        in  case icon of
                Icon_ {..} -> View Icon_ { classes = va : classes }
