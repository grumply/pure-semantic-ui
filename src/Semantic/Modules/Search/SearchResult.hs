module Semantic.Modules.Search.SearchResult where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (active,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.OnClick

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

instance HasAsProp (SearchResult item ms) where
    type AsProp (SearchResult item ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sr = sr { as = f }

instance HasAttributesProp (SearchResult item ms) where
    type Attribute (SearchResult item ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs sr = sr { attributes = cs }

instance HasClassesProp (SearchResult item ms) where
    getClasses = classes
    setClasses cs sr = sr { classes = cs }

instance HasActiveProp (SearchResult item ms) where
    getActive = active
    setActive a sr = sr { active = a }

pattern SearchResultItem :: Maybe item -> SearchResult item ms -> SearchResult item ms
pattern SearchResultItem i sr <- (item &&& id -> (i,sr)) where
    SearchResultItem i sr = sr { item = i }

instance HasOnClickProp (SearchResult item ms) where
    type OnClickProp (SearchResult item ms) = Maybe item -> Ef ms IO ()
    getOnClick = onClick
    setOnClick oc sr = sr { onClick = oc }

pattern RenderSearchResult :: (SearchResult item ms -> [View ms]) -> SearchResult item ms -> SearchResult item ms
pattern RenderSearchResult rsr sr <- (renderSearchResult &&& id -> (rsr,sr)) where
    RenderSearchResult rsr sr = sr { renderSearchResult = rsr }