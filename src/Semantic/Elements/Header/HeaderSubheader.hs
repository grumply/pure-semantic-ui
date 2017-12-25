module Semantic.Elements.Header.HeaderSubheader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data HeaderSubheader ms = HeaderSubheader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (HeaderSubheader ms) where
    def = (G.to gdef) { as = Div }

pattern HeaderSubheader :: Typeable ms => HeaderSubheader ms -> View ms
pattern HeaderSubheader hs = View hs

instance Typeable ms => Pure HeaderSubheader ms where
    render HeaderSubheader_ {..} =
        as
            ( ClassList ( "sub" : "header" : classes )
            : attributes
            )
            children