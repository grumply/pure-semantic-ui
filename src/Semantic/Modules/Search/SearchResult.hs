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
import Semantic.Properties.Renderer
import Semantic.Properties.Value

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

instance HasValueProp (SearchResult item ms) where
    type ValueProp (SearchResult item ms) = Maybe item
    getValue = item
    setValue v sr = sr { item = v }

instance HasOnClickProp (SearchResult item ms) where
    type OnClickProp (SearchResult item ms) = Maybe item -> Ef ms IO ()
    getOnClick = onClick
    setOnClick oc sr = sr { onClick = oc }

instance HasRendererProp (SearchResult item ms) where
    type RendererProp (SearchResult item ms) = SearchResult item ms -> [View ms]
    getRenderer = renderSearchResult
    setRenderer r sr = sr { renderSearchResult = r }