module Semantic.Modules.Search.SearchResult where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (active,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.OnClick

data SearchResult ms = SearchResult_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (SearchResult ms) where
    def = (G.to gdef) { as = Div }

pattern SearchResult :: SearchResult ms -> View ms
pattern SearchResult sr = View sr

instance Pure SearchResult ms where
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
                : onClick # On "click" def (\_ -> return $ Just onClick)
                : attributes
                )
                children

instance HasAsProp (SearchResult ms) where
    type AsProp (SearchResult ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sr = sr { as = f }

instance HasAttributesProp (SearchResult ms) where
    type Attribute (SearchResult ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs sr = sr { attributes = cs }

instance HasChildrenProp (SearchResult ms) where
    type Child (SearchResult ms) = View ms
    getChildren = children
    setChildren cs sr = sr { children = cs }

instance HasClassesProp (SearchResult ms) where
    getClasses = classes
    setClasses cs sr = sr { classes = cs }

instance HasActiveProp (SearchResult ms) where
    getActive = active
    setActive a sr = sr { active = a }

instance HasOnClickProp (SearchResult ms) where
    type OnClickProp (SearchResult ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc sr = sr { onClick = oc }