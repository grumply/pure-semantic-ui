module Semantic.Modules.Search.SearchResult where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (active,onClick)

import Semantic.Utils

data SearchResult item ms = SearchResult_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , active :: Bool
    , item :: Maybe item
    , onClick :: Maybe item -> Ef ms IO ()
    , renderSearchResult :: SearchResult item ms -> [View ms]
    } deriving (Generic)

instance Default (SearchResult item ms) where
    def = (G.to gdef) { as = Div }

pattern SearchResult :: Typeable item => SearchResult item ms -> View ms
pattern SearchResult sr = View sr

instance Typeable item => Pure (SearchResult item) ms where
    render sr@SearchResult_ {..} =
        let
            cs =
                ( active # "active"
                : "result"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs 
                : On "click" def (\_ -> return $ Just (onClick item))
                : attributes
                )
                (renderSearchResult sr)
