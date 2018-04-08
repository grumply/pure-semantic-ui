module Semantic.Modules.Dropdown where

import GHC.Generics as G
import Pure.Lifted (JSV,Node(..),(.#))
import Pure.View hiding (button,disabled,inline,onBlur,onClick,onFocus,simple)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasBasicProp(..), pattern Basic
  , HasIsButtonProp(..), pattern IsButton
  , HasCompactProp(..), pattern Compact
  , HasDisabledProp(..), pattern Disabled
  , HasErrorProp(..), pattern Error
  , HasFluidProp(..), pattern Fluid
  , HasFloatingProp(..), pattern Floating
  , HasInlineProp(..), pattern Inline
  , HasLabeledProp(..), pattern Labeled
  , HasLoadingProp(..), pattern Loading
  , HasIsItemProp(..), pattern IsItem
  , HasMultipleProp(..), pattern Multiple
  , HasOnBlurProp(..), pattern OnBlur
  , HasOnChangeProp(..), pattern OnChange
  , HasOnClickProp(..), pattern OnClick
  , HasOnFocusProp(..), pattern OnFocus
  , HasOnMouseDownProp(..), pattern OnMouseDown
  , HasOpenProp(..), pattern Open
  , HasPointingProp(..), pattern Pointing
  , HasIsSearchProp(..), pattern IsSearch
  , HasSelectionProp(..), pattern Selection
  , HasSimpleProp(..), pattern Simple
  , HasScrollingProp(..), pattern Scrolling
  , HasTabIndexProp(..), pattern TabIndex
  , HasUpwardProp(..), pattern Upward
  , HasActiveProp(..), pattern Active
  , HasSelectedProp(..), pattern Selected
  , HasInputRefProp(..), pattern InputRef
  , HasTabIndexProp(..), pattern TabIndex
  , HasTypeProp(..), pattern Type
  , HasValueProp(..), pattern Value
  )

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

data Divider ms = Divider_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Divider ms) where
    def = (G.to gdef) { as = Div }

pattern Divider :: Divider ms -> View ms
pattern Divider dd = View dd

instance Pure Divider ms where
    render Divider_ {..} =
        let
            cs =
                ( "divider"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                []

instance HasAsProp (Divider ms) where
    type AsProp (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f dd = dd { as = f }

instance HasAttributesProp (Divider ms) where
    type Attribute (Divider ms) = Feature ms
    getAttributes = attributes
    setAttributes cs dd = dd { attributes = cs }

instance HasClassesProp (Divider ms) where
    getClasses = classes
    setClasses cs dd = dd { classes = cs }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header dh = View dh

instance Pure Header ms where
    render Header_ {..} =
        let
            cs =
                ( "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f dh = dh { as = f }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes cs dh = dh { attributes = cs }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs dh = dh { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs dh = dh { classes = cs }

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Ef ms IO ()
    , selected :: Bool
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Item ms -> View ms
pattern Item di = View di

instance Pure (Item ) ms where
    render di@Item_ {..} =
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
                : attributes
                )
                children

instance HasAsProp (Item ms) where
    type AsProp (Item ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f di = di { as = f }

instance HasAttributesProp (Item ms) where
    type Attribute (Item ms) = Feature ms
    getAttributes = attributes
    setAttributes cs di = di { attributes = cs }

instance HasChildrenProp (Item ms) where
    type Child (Item ms) = View ms
    getChildren = children
    setChildren cs di = di { children = cs }

instance HasClassesProp (Item ms) where
    getClasses = classes
    setClasses cs di = di { classes = cs }

instance HasDisabledProp (Item ms) where
    getDisabled = disabled
    setDisabled d di = di { disabled = d }

instance HasOnClickProp (Item ms) where
    type OnClickProp (Item ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc di = di { onClick = oc }

instance HasSelectedProp (Item ms) where
    getSelected = selected
    setSelected s di = di { selected = s }

data Menu ms = Menu_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , scrolling :: Bool
    } deriving (Generic)

instance Default (Menu ms) where
    def = (G.to gdef) { as = Div }

pattern Menu :: Menu ms -> View ms
pattern Menu dm = View dm

instance Pure Menu ms where
    render Menu_ {..} =
        let
            cs =
                ( scrolling # "scrolling"
                : "menu transition"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Menu ms) where
    type AsProp (Menu ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f dm = dm { as = f }

instance HasAttributesProp (Menu ms) where
    type Attribute (Menu ms) = Feature ms
    getAttributes = attributes
    setAttributes cs dm = dm { attributes = cs }

instance HasChildrenProp (Menu ms) where
    type Child (Menu ms) = View ms
    getChildren = children
    setChildren cs dm = dm { children = cs }

instance HasClassesProp (Menu ms) where
    getClasses = classes
    setClasses cs dm = dm { classes = cs }

instance HasScrollingProp (Menu ms) where
    getScrolling = scrolling
    setScrolling s dm = dm { scrolling = s }

data SearchInput ms = SearchInput_
    { attributes :: [Feature ms]
    , classes :: [Txt]
    , inputRef :: JSV -> Ef ms IO ()
    , onChange :: Txt -> Ef ms IO ()
    , tabIndex :: Maybe Int
    , _type :: Txt
    , value :: Txt
    } deriving (Generic)

instance Default (SearchInput ms) where
    def = (G.to gdef) { _type = "text" }

pattern SearchInput :: SearchInput ms -> View ms
pattern SearchInput dh = View dh

instance Pure SearchInput ms where
    render SearchInput_ {..} =
        let
            handleChange = return . fmap onChange . ((.# "target") >=> (.# "value")) . evtObj

            cs =
                ( "search"
                : classes
                )
        in
            Input
                ( HTML.Value value
                : On "change" def handleChange
                : HostRef (\(Node n) -> return . Just $ inputRef n)
                : may Tabindex tabIndex
                : HTML.Type _type
                : Attribute "autoComplete" "off"
                : mergeClasses (ClassList cs : attributes)
                )
                []

instance HasAttributesProp (SearchInput ms) where
    type Attribute (SearchInput ms) = Feature ms
    getAttributes = attributes
    setAttributes cs dsi = dsi { attributes = cs }

instance HasClassesProp (SearchInput ms) where
    getClasses = classes
    setClasses cs dsi = dsi { classes = cs }

instance HasInputRefProp (SearchInput ms) where
    type InputRefProp (SearchInput ms) = JSV -> Ef ms IO ()
    getInputRef = inputRef
    setInputRef ir dsi = dsi { inputRef = ir }

instance HasOnChangeProp (SearchInput ms) where
    type OnChangeProp (SearchInput ms) = Txt -> Ef ms IO ()
    getOnChange = onChange
    setOnChange oc dsi = dsi { onChange = oc }

instance HasTabIndexProp (SearchInput ms) where
    getTabIndex = tabIndex
    setTabIndex ti dsi = dsi { tabIndex = ti }

instance HasTypeProp (SearchInput ms) where
    getType = _type
    setType t dsi = dsi { _type = t }

instance HasValueProp (SearchInput ms) where
    getValue = value
    setValue v dsi = dsi { value = v }
