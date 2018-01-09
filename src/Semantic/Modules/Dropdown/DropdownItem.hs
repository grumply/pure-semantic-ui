module Semantic.Modules.Dropdown.DropdownItem where

import GHC.Generics as G
import Pure.View hiding (disabled,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Disabled
import Semantic.Properties.OnClick
import Semantic.Properties.Renderer
import Semantic.Properties.Selected
import Semantic.Properties.Value

data DropdownItem item ms = DropdownItem_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Maybe item -> Ef ms IO ()
    , selected :: Bool
    , item :: Maybe item
    , renderDropdownItem :: Maybe item -> [View ms]
    } deriving (Generic)

instance Default (DropdownItem item ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownItem :: Typeable item => DropdownItem item ms -> View ms
pattern DropdownItem di = View di

instance Typeable item => Pure (DropdownItem item) ms where
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
                : On "click" def (\_ -> return $ Just $ onClick item)
                : Role "option"
                : Attribute "aria-disabled" (disabled ? "true" $ "false")
                : Attribute "aria-active" (active ? "true" $ "false")
                : Attribute "aria-selected" (selected ? "true" $ "false")
                : attributes
                )
                (renderDropdownItem item)

instance HasAsProp (DropdownItem item ms) where
    type AsProp (DropdownItem item ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f di = di { as = f }

instance HasAttributesProp (DropdownItem item ms) where
    type Attribute (DropdownItem item ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs di = di { attributes = cs }

instance HasClassesProp (DropdownItem item ms) where
    getClasses = classes
    setClasses cs di = di { classes = cs }

instance HasDisabledProp (DropdownItem item ms) where
    getDisabled = disabled
    setDisabled d di = di { disabled = d }

instance HasOnClickProp (DropdownItem item ms) where
    type OnClickProp (DropdownItem item ms) = Maybe item -> Ef ms IO ()
    getOnClick = onClick
    setOnClick oc di = di { onClick = oc }

instance HasSelectedProp (DropdownItem item ms) where
    getSelected = selected
    setSelected s di = di { selected = s }

instance HasValueProp (DropdownItem item ms) where
    type ValueProp (DropdownItem item ms) = Maybe item
    getValue = item
    setValue v di = di { item = v }

instance HasRendererProp (DropdownItem item ms) where
    type RendererProp (DropdownItem item ms) = Maybe item -> [View ms]
    getRenderer = renderDropdownItem
    setRenderer r di = di { renderDropdownItem = r }