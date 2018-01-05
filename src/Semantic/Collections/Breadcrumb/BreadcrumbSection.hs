module Semantic.Collections.Breadcrumb.BreadcrumbSection where

import GHC.Generics as G
import Pure.View hiding (active,name,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Ref
import Semantic.Properties.Link
import Semantic.Properties.OnClick

data BreadcrumbSection ms = BreadcrumbSection_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (BreadcrumbSection ms) where
    def = (G.to gdef) { as = Div }

pattern BreadcrumbSection :: Typeable ms => BreadcrumbSection ms -> View ms
pattern BreadcrumbSection bcs = View bcs

instance Typeable ms => Pure BreadcrumbSection ms where
    render BreadcrumbSection_ {..} =
        let
            e = link ? A $ ref ? A $ as
            cs = 
                ( active # "active"
                : "section"
                : classes
                )
        in
            e
                ( mergeClasses $ ClassList cs
                : ref
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasAsProp (BreadcrumbSection ms) where
    type AsProp (BreadcrumbSection ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a bcs = bcs { as = a }

instance HasAttributesProp (BreadcrumbSection ms) where
    type Attribute (BreadcrumbSection ms) = Feature ms
    getAttributes = attributes
    setAttributes as bcs = bcs { attributes = as }

instance HasChildrenProp (BreadcrumbSection ms) where
    type Child (BreadcrumbSection ms) = View ms
    getChildren = children
    setChildren cs bcs = bcs { children = cs }

instance HasClassesProp (BreadcrumbSection ms) where
    getClasses = classes
    setClasses cs bcs = bcs { classes = cs }

instance HasActiveProp (BreadcrumbSection ms) where
    getActive = active
    setActive a bcs = bcs { active = a }

instance HasRefProp (BreadcrumbSection ms) where
    type RefProp (BreadcrumbSection ms) = Feature ms
    getRef = ref
    setRef r bcs = bcs { ref = r }

instance HasLinkProp (BreadcrumbSection ms) where
    getLink = link
    setLink l bcs = bcs { link = l }

instance HasOnClickProp (BreadcrumbSection ms) where
    type OnClickProp (BreadcrumbSection ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc bcs = bcs { onClick = oc }