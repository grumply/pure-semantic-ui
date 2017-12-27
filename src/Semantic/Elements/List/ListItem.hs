module Semantic.Elements.List.ListItem where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

import Semantic.Properties.Active
import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes

data ListItem ms = ListItem_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , click :: Ef ms IO ()
    , value :: Txt
    } deriving (Generic)

instance Default (ListItem ms) where
    def = (G.to gdef) { as = Div }

pattern ListItem :: Typeable ms => ListItem ms -> View ms
pattern ListItem li = View li

instance Typeable ms => Pure ListItem ms where
    render ListItem_ {..} =
        let
            li = 
                case as [] [] of
                    Li _ _ -> True
                    _      -> False
            cs =
                ( active # "active"
                : disabled # "disabled"
                : (not li) # "item"
                : classes
                )

            valueProp = li ? Value value $ Prop "data-value" value
        in
            as
                ( onClick click
                : valueProp
                : ClassList cs
                : Role "listitem"
                : attributes
                )
                children
    
instance HasActiveProp (ListItem ms) where
    getActive = active
    setActive a li = li { active = a }

instance HasAsProp (ListItem ms) where
    type AsProp (ListItem ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f li = li { as = f }

instance HasAttributesProp (ListItem ms) where
    type Attribute (ListItem ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs li = li { attributes = cs }

instance HasChildrenProp (ListItem ms) where
    type Child (ListItem ms) = View ms
    getChildren = children
    setChildren cs li = li { children = cs }

instance HasClassesProp (ListItem ms) where
    getClasses = classes
    setClasses cs li = li { classes = cs }