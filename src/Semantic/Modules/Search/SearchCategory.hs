module Semantic.Modules.Search.SearchCategory where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (name,active)

import Semantic.Utils

data SearchCategory result ms = SearchCategory_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , name :: Txt
    , results :: [result]
    , renderSearchCategory :: SearchCategory result ms -> [View ms]
    } deriving (Generic)

instance Default (SearchCategory result ms) where
    def = (G.to gdef) 
        { as = Div 
        , renderSearchCategory = fromTxt . name
        }

pattern SearchCategory :: Typeable result => SearchCategory result ms -> View ms
pattern SearchCategory sc = View sc

instance Typeable result => Pure (SearchCategory result) ms where
    render sc@SearchCategory_ {..} =
        let
            cs = 
                ( active # "active"
                : "category"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                ( Div [ ClassList [ "name" ] ] (renderSearchCategory sc)
                : children 
                )
