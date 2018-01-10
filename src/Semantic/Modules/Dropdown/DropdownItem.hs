module Semantic.Modules.Dropdown.DropdownItem where

import GHC.Generics as G
import Pure.View hiding (disabled,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Disabled
import Semantic.Properties.OnClick
import Semantic.Properties.Selected

data DropdownItem ms = DropdownItem_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Ef ms IO ()
    , selected :: Bool
    } deriving (Generic)

instance Default (DropdownItem ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownItem :: DropdownItem ms -> View ms
pattern DropdownItem di = View di

instance Pure (DropdownItem ) ms where
    render di@DropdownItem_ {..} =
        let
            cs = 
                ( active # "active"
                : disabled # "disabled"
                : selected # "selected"
                : "item"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : onClick # On "click" def (\_ -> return $ Just onClick)
                : Role "option"
                : Attribute "aria-disabled" (disabled ? "true" $ "false")
                : Attribute "aria-active" (active ? "true" $ "false")
                : Attribute "aria-selected" (selected ? "true" $ "false")
                : attributes
                )
                children

instance HasAsProp (DropdownItem ms) where
    type AsProp (DropdownItem ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f di = di { as = f }

instance HasAttributesProp (DropdownItem ms) where
    type Attribute (DropdownItem ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs di = di { attributes = cs }

instance HasChildrenProp (DropdownItem ms) where
    type Child (DropdownItem ms) = View ms
    getChildren = children
    setChildren cs di = di { children = cs }

instance HasClassesProp (DropdownItem ms) where
    getClasses = classes
    setClasses cs di = di { classes = cs }

instance HasDisabledProp (DropdownItem ms) where
    getDisabled = disabled
    setDisabled d di = di { disabled = d }

instance HasOnClickProp (DropdownItem ms) where
    type OnClickProp (DropdownItem ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc di = di { onClick = oc }

instance HasSelectedProp (DropdownItem ms) where
    getSelected = selected
    setSelected s di = di { selected = s }