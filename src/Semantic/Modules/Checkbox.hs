module Semantic.Modules.Checkbox where

import GHC.Generics as G
import Pure.View hiding (disabled,name,onClick,Checked,Name,Type,Value)
import qualified Pure.View as HTML
import Pure.Lifted (Node)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Disabled
import Semantic.Properties.Fitted
import Semantic.Properties.IsChecked
import Semantic.Properties.Name
import Semantic.Properties.OnChange
import Semantic.Properties.OnClick
import Semantic.Properties.OnMouseDown
import Semantic.Properties.WithRef
import Semantic.Properties.IsRadio
import Semantic.Properties.ReadOnly
import Semantic.Properties.Slider
import Semantic.Properties.TabIndex
import Semantic.Properties.Toggle
import Semantic.Properties.Type
import Semantic.Properties.Value

data Checkbox ms = Checkbox_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , checked :: Checked
    , disabled :: Bool
    , fitted :: Bool
    , name :: Txt
    , onChange :: Checkbox ms -> Ef ms IO ()
    , onClick :: Checkbox ms -> Ef ms IO ()
    , onMouseDown :: Checkbox ms -> Ef ms IO ()
    , withRef :: Node -> Ef ms IO ()
    , radio :: Bool
    , readOnly :: Bool
    , slider :: Bool
    , tabIndex :: Maybe Int
    , toggle :: Bool
    , _type :: Txt
    , value :: Txt
    } deriving (Generic)

instance Default (Checkbox ms) where
    def = (G.to gdef) { as = Div }

pattern Checkbox :: Checkbox ms -> View ms
pattern Checkbox c = View c

pattern Radio :: Checkbox ms -> View ms
pattern Radio r = View (Type "radio" (IsRadio r))

renderChecked Checked = HTML.Checked True
renderChecked Indeterminate = HTML.Checked False
renderChecked Unchecked = nil

instance Pure Checkbox ms where
    render cb@Checkbox_ {..} =
        let
            cs =
                ( "ui"
                : (checked == Checked) # "checked"
                : disabled # "disabled"
                : (checked == Indeterminate) # "indeterminate"
                : fitted # "fitted" 
                : radio # "radio"
                : readOnly # "read-only"
                : slider # "slider"
                : toggle # "toggle"
                : "checkbox"
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : On "change" def (\_ -> return $ Just (onChange cb))
                : On "click" def (\_ -> return $ Just (onClick cb))
                : On "mousedown" def (\_ -> return $ Just (onMouseDown cb))
                : attributes
                )
                ( HTML.Input
                    [ ClassList [ "hidden" ]
                    , HostRef (return . Just . withRef)
                    , renderChecked checked
                    , HTML.Name name
                    , Readonly readOnly
                    , may (\ti -> Tabindex (disabled ? (-1) $ ti)) tabIndex
                    , HTML.Type _type
                    , HTML.Value value
                    ]
                    []
                : children
                )

instance HasAsProp (Checkbox ms) where
    type AsProp (Checkbox ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cb = cb { as = a }

instance HasAttributesProp (Checkbox ms) where
    type Attribute (Checkbox ms) = Feature ms
    getAttributes = attributes
    setAttributes as cb = cb { attributes = as }

instance HasChildrenProp (Checkbox ms) where
    type Child (Checkbox ms) = View ms
    getChildren = children
    setChildren cs cb = cb { children = cs }

instance HasClassesProp (Checkbox ms) where
    getClasses = classes
    setClasses cs cb = cb { classes = cs }

instance HasDisabledProp (Checkbox ms) where
    getDisabled = disabled
    setDisabled d cb = cb { disabled = d }

instance HasFittedProp (Checkbox ms) where
    getFitted = fitted
    setFitted f cb = cb { fitted = f }

instance HasIsCheckedProp (Checkbox ms) where
    getIsChecked = checked
    setIsChecked c cb = cb { checked = c }

instance HasNameProp (Checkbox ms) where
    getName = name
    setName n cb = cb { name = n }

instance HasOnChangeProp (Checkbox ms) where
    type OnChangeProp (Checkbox ms) = Checkbox ms -> Ef ms IO ()
    getOnChange = onChange
    setOnChange oc cb = cb { onChange = oc }

instance HasOnClickProp (Checkbox ms) where
    type OnClickProp (Checkbox ms) = Checkbox ms -> Ef ms IO ()
    getOnClick = onClick
    setOnClick oc cb = cb { onClick = oc }

instance HasOnMouseDownProp (Checkbox ms) where
    type OnMouseDownProp (Checkbox ms) = Checkbox ms -> Ef ms IO ()
    getOnMouseDown = onMouseDown
    setOnMouseDown omd cb = cb { onMouseDown = omd }

instance HasWithRefProp (Checkbox ms) where
    type WithRefProp (Checkbox ms) = Node -> Ef ms IO ()
    getWithRef = withRef
    setWithRef wr cb = cb { withRef = wr }

instance HasIsRadioProp (Checkbox ms) where
    getIsRadio = radio
    setIsRadio r cb = cb { radio = r }

instance HasReadOnlyProp (Checkbox ms) where
    getReadOnly = readOnly
    setReadOnly ro cb = cb { readOnly = ro }

instance HasSliderProp (Checkbox ms) where
    getSlider = slider
    setSlider s cb = cb { slider = s }

instance HasTabIndexProp (Checkbox ms) where
    getTabIndex = tabIndex
    setTabIndex ti cb = cb { tabIndex = ti }

instance HasToggleProp (Checkbox ms) where
    getToggle = toggle
    setToggle t cb = cb { toggle = t }

instance HasTypeProp (Checkbox ms) where
    getType = _type
    setType t cb = cb { _type = t }

instance HasValueProp (Checkbox ms) where
    getValue = value
    setValue v cb = cb { value = v }

