module Semantic.Modules.Dropdown (module Semantic.Modules.Dropdown, module Export) where

import GHC.Generics as G
import Pure.View hiding (button,disabled,inline,onBlur,onClick,onFocus,simple)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Basic
import Semantic.Properties.IsButton
import Semantic.Properties.Compact
import Semantic.Properties.Disabled
import Semantic.Properties.Error
import Semantic.Properties.Fluid
import Semantic.Properties.Floating
import Semantic.Properties.Inline
import Semantic.Properties.Labeled
import Semantic.Properties.Loading
import Semantic.Properties.IsItem
import Semantic.Properties.Multiple
import Semantic.Properties.OnBlur
import Semantic.Properties.OnChange
import Semantic.Properties.OnClick
import Semantic.Properties.OnFocus
import Semantic.Properties.OnMouseDown
import Semantic.Properties.Open
import Semantic.Properties.Pointing
import Semantic.Properties.IsSearch
import Semantic.Properties.Selection
import Semantic.Properties.Simple
import Semantic.Properties.Scrolling
import Semantic.Properties.TabIndex
import Semantic.Properties.Upward

import Semantic.Modules.Dropdown.DropdownDivider as Export
import Semantic.Modules.Dropdown.DropdownHeader as Export
import Semantic.Modules.Dropdown.DropdownItem as Export
import Semantic.Modules.Dropdown.DropdownMenu as Export
import Semantic.Modules.Dropdown.DropdownSearchInput as Export

import Prelude hiding (error)

{-
Approaching this differently than Semantic-UI-React. Instead of managing
everything internally, I want this to be a pure component that is maximally
extensible so that customized dropdown components can be built on top 
of it without too much work. The Semantic-UI-React/dropdown component should
be implementable with this approach with this pure component as a core.

I will likely split managed dropdown components off into their own library
similarly to semantic-ui-pure-forms.
-}

data Dropdown ms = Dropdown_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , basic :: Bool
    , button :: Bool
    , compact :: Bool
    , disabled :: Bool
    , error :: Bool
    , fluid :: Bool
    , floating :: Bool
    , inline :: Bool
    , labeled :: Bool
    , loading :: Bool
    , item :: Bool
    , multiple :: Bool
    , onBlur :: Ef ms IO ()
    , onChange :: Ef ms IO ()
    , onClick :: Ef ms IO ()
    , onFocus :: Ef ms IO ()
    , onMouseDown :: Ef ms IO ()
    , open :: Bool
    , pointing :: Maybe Txt
    , search :: Bool
    , selection :: Bool
    , simple :: Bool
    , scrolling :: Bool
    , tabIndex :: Maybe Int
    , upward :: Bool
    } deriving (Generic)

instance Default (Dropdown ms) where
    def = (G.to gdef) { as = Div }

pattern Dropdown :: Dropdown ms -> View ms
pattern Dropdown d = View d

pattern Select :: Dropdown ms -> View ms
pattern Select d = View (Selection d)

instance Pure Dropdown ms where
    render Dropdown_ {..} =
        let 
            cs =
                ( "ui"
                : open # "active visible"
                : disabled # "disabled"
                : error # "error"
                : loading # "loading"
                : basic # "basic"
                : button # "button"
                : compact # "compact"
                : fluid # "fluid"
                : floating # "floating"
                : inline # "inline"
                : labeled # "labeled"
                : item # "item"
                : multiple # "multiple"
                : search # "search"
                : selection # "selection"
                : simple # "simple"
                : scrolling # "scrolling"
                : upward # "upward"
                : may (<<>> "pointing") pointing
                : "dropdown"
                : classes
                )
        in as
                ( mergeClasses $ ClassList cs
                : onBlur # On "blur" def (\_ -> return $ Just onBlur)
                : onChange # On "change" def (\_ -> return $ Just onChange)
                : onClick # On "click" def (\_ -> return $ Just onClick)
                : onFocus # On "focus" def (\_ -> return $ Just onFocus)
                : onMouseDown # On "mousedown" def (\_ -> return $ Just onMouseDown)
                : may Tabindex tabIndex
                : attributes
                )
                children

instance HasAsProp (Dropdown ms) where
    type AsProp (Dropdown ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f dd = dd { as = f }

instance HasAttributesProp (Dropdown ms) where
    type Attribute (Dropdown ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs dd = dd { attributes = cs }

instance HasChildrenProp (Dropdown ms) where
    type Child (Dropdown ms) = View ms
    getChildren = children
    setChildren cs dd = dd { children = cs }

instance HasClassesProp (Dropdown ms) where
    getClasses = classes
    setClasses cs dd = dd { classes = cs }

instance HasBasicProp (Dropdown ms) where
    getBasic = basic
    setBasic b dd = dd { basic = b }

instance HasIsButtonProp (Dropdown ms) where
    getIsButton = button
    setIsButton b dd = dd { button = b }

instance HasCompactProp (Dropdown ms) where
    getCompact = compact
    setCompact c dd = dd { compact = c }

instance HasDisabledProp (Dropdown ms) where
    getDisabled = disabled
    setDisabled d dd = dd { disabled = d }

instance HasErrorProp (Dropdown ms) where
    getError = error
    setError e dd = dd { error = e }

instance HasFluidProp (Dropdown ms) where
    getFluid = fluid
    setFluid f dd = dd { fluid = f }

instance HasFloatingProp (Dropdown ms) where
    getFloating = floating
    setFloating f dd = dd { floating = f }

instance HasInlineProp (Dropdown ms) where
    type InlineProp (Dropdown ms) = Bool
    getInline = inline
    setInline i dd = dd { inline = i }

instance HasLabeledProp (Dropdown ms) where
    getLabeled = labeled
    setLabeled l dd = dd { labeled = l }

instance HasLoadingProp (Dropdown ms) where
    getLoading = loading
    setLoading l dd = dd { loading = l }

instance HasIsItemProp (Dropdown ms) where
    getIsItem = item
    setIsItem i dd = dd { item = i }

instance HasMultipleProp (Dropdown ms) where
    getMultiple = multiple
    setMultiple m dd = dd { multiple = m }

instance HasOnBlurProp (Dropdown ms) where
    type OnBlurProp (Dropdown ms) = Ef ms IO ()
    getOnBlur = onBlur
    setOnBlur ob dd = dd { onBlur = ob }

instance HasOnChangeProp (Dropdown ms) where
    type OnChangeProp (Dropdown ms) = Ef ms IO ()
    getOnChange = onChange
    setOnChange oc dd = dd { onChange = oc }

instance HasOnClickProp (Dropdown ms) where
    type OnClickProp (Dropdown ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc dd = dd { onClick = oc }

instance HasOnFocusProp (Dropdown ms) where
    type OnFocusProp (Dropdown ms) = Ef ms IO ()
    getOnFocus = onFocus
    setOnFocus onf dd = dd { onFocus = onf }

instance HasOnMouseDownProp (Dropdown ms) where
    type OnMouseDownProp (Dropdown ms) = Ef ms IO ()
    getOnMouseDown = onMouseDown
    setOnMouseDown omd dd = dd { onMouseDown = omd }

instance HasOpenProp (Dropdown ms) where
    getOpen = open
    setOpen o dd = dd { open = o }

instance HasPointingProp (Dropdown ms) where
    getPointing = pointing
    setPointing p dd = dd { pointing = p }

instance HasIsSearchProp (Dropdown ms) where
    getIsSearch = search
    setIsSearch is dd = dd { search = is }

instance HasSelectionProp (Dropdown ms) where
    getSelection = selection
    setSelection s dd = dd { selection = s }

instance HasSimpleProp (Dropdown ms) where
    getSimple = simple
    setSimple s dd = dd { simple = s }

instance HasScrollingProp (Dropdown ms) where
    getScrolling = scrolling
    setScrolling s dd = dd { scrolling = s }

instance HasTabIndexProp (Dropdown ms) where
    getTabIndex = tabIndex
    setTabIndex ti dd = dd { tabIndex = ti }

instance HasUpwardProp (Dropdown ms) where
    getUpward = upward
    setUpward u dd = dd { upward = u }