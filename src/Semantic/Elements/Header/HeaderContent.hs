module Semantic.Elements.Header.HeaderContent where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data HeaderContent ms = HeaderContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (HeaderContent ms) where
    def = (G.to gdef) { as = Div }

pattern HeaderContent :: Typeable ms => HeaderContent ms -> View ms
pattern HeaderContent hc = View hc

instance Typeable ms => Pure HeaderContent ms where
    render HeaderContent_ {..} =
        as 
            ( ClassList ( "content" : classes) 
            : attributes 
            ) 
            children