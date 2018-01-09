module Semantic.Modules.Search.SearchCategory where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (name,active)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Name
import Semantic.Properties.Results

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

instance HasAsProp (SearchCategory result ms) where
    type AsProp (SearchCategory result ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sc = sc { as = f }

instance HasAttributesProp (SearchCategory result ms) where
    type Attribute (SearchCategory result ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs sc = sc { attributes = cs }

instance HasChildrenProp (SearchCategory result ms) where
    type Child (SearchCategory result ms) = View ms
    getChildren = children
    setChildren cs sc = sc { children = cs }

instance HasClassesProp (SearchCategory result ms) where
    getClasses = classes
    setClasses cs sc = sc { classes = cs }

instance HasActiveProp (SearchCategory result ms) where
    getActive = active
    setActive a sc = sc { active = a }

instance HasNameProp (SearchCategory result ms) where
    getName = name
    setName n sc = sc { name = n }

instance HasResultsProp (SearchCategory result ms) where
    type ResultsProp (SearchCategory result ms) = [result]
    getResults = results
    setResults rs sc = sc { results = rs }

pattern RenderSearchCategory :: (SearchCategory result ms -> [View ms]) -> SearchCategory result ms -> SearchCategory result ms
pattern RenderSearchCategory rsc sc <- (renderSearchCategory &&& id -> (rsc,sc)) where
    RenderSearchCategory rsc sc = sc { renderSearchCategory = rsc }
