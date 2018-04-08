module Semantic.Elements.List.ListItem where

import GHC.Generics as G
import Pure.View hiding (active,disabled,onClick,Value)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasActiveProp(..), pattern Active
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasDisabledProp(..), pattern Disabled
  , HasOnClickProp(..), pattern OnClick
  , HasValueProp(..), pattern Value
  )

data ListItem ms = ListItem_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Ef ms IO ()
    , value :: Txt
    } deriving (Generic)

instance Default (ListItem ms) where
    def = (G.to gdef) { as = Div }

pattern ListItem :: ListItem ms -> View ms
pattern ListItem li = View li

instance Pure ListItem ms where
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

            valueProp = li ? HTML.Value value $ Prop "data-value" value
        in
            as
                ( mergeClasses $ HTML.onClick onClick
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

instance HasOnClickProp (ListItem ms) where
    type OnClickProp (ListItem ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc li = li { onClick = oc }

instance HasDisabledProp (ListItem ms) where
    getDisabled = disabled
    setDisabled d li = li { disabled = d }

instance HasValueProp (ListItem ms) where
    getValue = value
    setValue v li = li { value = v }
