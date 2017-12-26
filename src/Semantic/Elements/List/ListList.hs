module Semantic.Elements.List.ListList where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

data ListList ms = ListList_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ListList ms) where
    def = (G.to gdef) { as = Div }

pattern ListList :: Typeable ms => ListList ms -> View ms
pattern ListList ll = View ll

instance Typeable ms => Pure ListList ms where
    render ListList_ {..} =
        let
            proxy =
                case as [] [] of
                    Ul _ _ -> False
                    Ol _ _ -> False
                    _      -> True

        in
            as 
                ( ClassList ( proxy # "list" : classes )
                : attributes
                )
                children