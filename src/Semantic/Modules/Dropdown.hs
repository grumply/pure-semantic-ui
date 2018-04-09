module Semantic.Modules.Dropdown
  ( module Properties
  , module Tools
  , Dropdown(..), pattern Dropdown
  , Divider(..), pattern Divider
  , Header(..), pattern Header
  , Item(..), pattern Item
  , Menu(..), pattern Menu
  , SearchInput, pattern SearchInput
  ) where

import GHC.Generics as G
import Pure.Lifted (JSV,Node(..),(.#))
import Pure.View hiding (button,disabled,inline,onBlur,onClick,onFocus,simple,Header,Menu,active,Value)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Basic, Basic(..)
  , pattern IsButton, IsButton(..)
  , pattern Compact, Compact(..)
  , pattern Disabled, Disabled(..)
  , pattern Error, Error(..)
  , pattern Fluid, Fluid(..)
  , pattern Floating, Floating(..)
  , pattern Inline, Inline(..)
  , pattern Labeled, Labeled(..)
  , pattern Loading, Loading(..)
  , pattern IsItem, IsItem(..)
  , pattern Multiple, Multiple(..)
  , pattern OnBlur, OnBlur(..)
  , pattern OnChange, OnChange(..)
  , pattern OnClick, OnClick(..)
  , pattern OnFocus, OnFocus(..)
  , pattern OnMouseDown, OnMouseDown(..)
  , pattern Open, Open(..)
  , pattern Pointing, Pointing(..)
  , pattern IsSearch, IsSearch(..)
  , pattern Selection, Selection(..)
  , pattern Simple, Simple(..)
  , pattern Scrolling, Scrolling(..)
  , pattern TabIndex, TabIndex(..)
  , pattern Upward, Upward(..)
  , pattern Active, Active(..)
  , pattern Selected, Selected(..)
  , pattern InputRef, InputRef(..)
  , pattern TabIndex, TabIndex(..)
  , pattern Type, Type(..)
  , pattern Value, Value(..)
  )

import Prelude hiding (error,Floating)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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

instance HasProp As (Dropdown ms) where
    type Prop As (Dropdown ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f dd = dd { as = f }

instance HasProp Attributes (Dropdown ms) where
    type Prop Attributes (Dropdown ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs dd = dd { attributes = cs }

instance HasProp Children (Dropdown ms) where
    type Prop Children (Dropdown ms) = [View ms]
    getProp _ = children
    setProp _ cs dd = dd { children = cs }

instance HasProp Classes (Dropdown ms) where
    type Prop Classes (Dropdown ms) = [Txt]
    getProp _ = classes
    setProp _ cs dd = dd { classes = cs }

instance HasProp Basic (Dropdown ms) where
    type Prop Basic (Dropdown ms) = Bool
    getProp _ = basic
    setProp _ b dd = dd { basic = b }

instance HasProp IsButton (Dropdown ms) where
    type Prop IsButton (Dropdown ms) = Bool
    getProp _ = button
    setProp _ b dd = dd { button = b }

instance HasProp Compact (Dropdown ms) where
    type Prop Compact (Dropdown ms) = Bool
    getProp _ = compact
    setProp _ c dd = dd { compact = c }

instance HasProp Disabled (Dropdown ms) where
    type Prop Disabled (Dropdown ms) = Bool
    getProp _ = disabled
    setProp _ d dd = dd { disabled = d }

instance HasProp Error (Dropdown ms) where
    type Prop Error (Dropdown ms) = Bool
    getProp _ = error
    setProp _ e dd = dd { error = e }

instance HasProp Fluid (Dropdown ms) where
    type Prop Fluid (Dropdown ms) = Bool
    getProp _ = fluid
    setProp _ f dd = dd { fluid = f }

instance HasProp Floating (Dropdown ms) where
    type Prop Floating (Dropdown ms) = Bool
    getProp _ = floating
    setProp _ f dd = dd { floating = f }

instance HasProp Inline (Dropdown ms) where
    type Prop Inline (Dropdown ms) = Bool
    getProp _ = inline
    setProp _ i dd = dd { inline = i }

instance HasProp Labeled (Dropdown ms) where
    type Prop Labeled (Dropdown ms) = Bool
    getProp _ = labeled
    setProp _ l dd = dd { labeled = l }

instance HasProp Loading (Dropdown ms) where
    type Prop Loading (Dropdown ms) = Bool
    getProp _ = loading
    setProp _ l dd = dd { loading = l }

instance HasProp IsItem (Dropdown ms) where
    type Prop IsItem (Dropdown ms) = Bool
    getProp _ = item
    setProp _ i dd = dd { item = i }

instance HasProp Multiple (Dropdown ms) where
    type Prop Multiple (Dropdown ms) = Bool
    getProp _ = multiple
    setProp _ m dd = dd { multiple = m }

instance HasProp OnBlur (Dropdown ms) where
    type Prop OnBlur (Dropdown ms) = Ef ms IO ()
    getProp _ = onBlur
    setProp _ ob dd = dd { onBlur = ob }

instance HasProp OnChange (Dropdown ms) where
    type Prop OnChange (Dropdown ms) = Ef ms IO ()
    getProp _ = onChange
    setProp _ oc dd = dd { onChange = oc }

instance HasProp OnClick (Dropdown ms) where
    type Prop OnClick (Dropdown ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc dd = dd { onClick = oc }

instance HasProp OnFocus (Dropdown ms) where
    type Prop OnFocus (Dropdown ms) = Ef ms IO ()
    getProp _ = onFocus
    setProp _ onf dd = dd { onFocus = onf }

instance HasProp OnMouseDown (Dropdown ms) where
    type Prop OnMouseDown (Dropdown ms) = Ef ms IO ()
    getProp _ = onMouseDown
    setProp _ omd dd = dd { onMouseDown = omd }

instance HasProp Open (Dropdown ms) where
    type Prop Open (Dropdown ms) = Bool
    getProp _ = open
    setProp _ o dd = dd { open = o }

instance HasProp Pointing (Dropdown ms) where
    type Prop Pointing (Dropdown ms) = Maybe Txt
    getProp _ = pointing
    setProp _ p dd = dd { pointing = p }

instance HasProp IsSearch (Dropdown ms) where
    type Prop IsSearch (Dropdown ms) = Bool
    getProp _ = search
    setProp _ is dd = dd { search = is }

instance HasProp Selection (Dropdown ms) where
    type Prop Selection (Dropdown ms) = Bool
    getProp _ = selection
    setProp _ s dd = dd { selection = s }

instance HasProp Simple (Dropdown ms) where
    type Prop Simple (Dropdown ms) = Bool
    getProp _ = simple
    setProp _ s dd = dd { simple = s }

instance HasProp Scrolling (Dropdown ms) where
    type Prop Scrolling (Dropdown ms) = Bool
    getProp _ = scrolling
    setProp _ s dd = dd { scrolling = s }

instance HasProp TabIndex (Dropdown ms) where
    type Prop TabIndex (Dropdown ms) = Maybe Int
    getProp _ = tabIndex
    setProp _ ti dd = dd { tabIndex = ti }

instance HasProp Upward (Dropdown ms) where
    type Prop Upward (Dropdown ms) = Bool
    getProp _ = upward
    setProp _ u dd = dd { upward = u }

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

instance HasProp As (Divider ms) where
    type Prop As (Divider ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f dd = dd { as = f }

instance HasProp Attributes (Divider ms) where
    type Prop Attributes (Divider ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs dd = dd { attributes = cs }

instance HasProp Classes (Divider ms) where
    type Prop Classes (Divider ms) = [Txt]
    getProp _ = classes
    setProp _ cs dd = dd { classes = cs }

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

instance HasProp As (Header ms) where
    type Prop As (Header ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f dh = dh { as = f }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs dh = dh { attributes = cs }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs dh = dh { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs dh = dh { classes = cs }

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

instance HasProp Active (Item ms) where
    type Prop Active (Item ms) = Bool
    getProp _ = active
    setProp _ a di = di { active = a }

instance HasProp As (Item ms) where
    type Prop As (Item ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f di = di { as = f }

instance HasProp Attributes (Item ms) where
    type Prop Attributes (Item ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs di = di { attributes = cs }

instance HasProp Children (Item ms) where
    type Prop Children (Item ms) = [View ms]
    getProp _ = children
    setProp _ cs di = di { children = cs }

instance HasProp Classes (Item ms) where
    type Prop Classes (Item ms) = [Txt]
    getProp _ = classes
    setProp _ cs di = di { classes = cs }

instance HasProp Disabled (Item ms) where
    type Prop Disabled (Item ms) = Bool
    getProp _ = disabled
    setProp _ d di = di { disabled = d }

instance HasProp OnClick (Item ms) where
    type Prop OnClick (Item ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc di = di { onClick = oc }

instance HasProp Selected (Item ms) where
    type Prop Selected (Item ms) = Bool
    getProp _ = selected
    setProp _ s di = di { selected = s }

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

instance HasProp As (Menu ms) where
    type Prop As (Menu ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f dm = dm { as = f }

instance HasProp Attributes (Menu ms) where
    type Prop Attributes (Menu ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs dm = dm { attributes = cs }

instance HasProp Children (Menu ms) where
    type Prop Children (Menu ms) = [View ms]
    getProp _ = children
    setProp _ cs dm = dm { children = cs }

instance HasProp Classes (Menu ms) where
    type Prop Classes (Menu ms) = [Txt]
    getProp _ = classes
    setProp _ cs dm = dm { classes = cs }

instance HasProp Scrolling (Menu ms) where
    type Prop Scrolling (Menu ms) = Bool
    getProp _ = scrolling
    setProp _ s dm = dm { scrolling = s }

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

instance HasProp Attributes (SearchInput ms) where
    type Prop Attributes (SearchInput ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs dsi = dsi { attributes = cs }

instance HasProp Classes (SearchInput ms) where
    type Prop Classes (SearchInput ms) = [Txt]
    getProp _ = classes
    setProp _ cs dsi = dsi { classes = cs }

instance HasProp InputRef (SearchInput ms) where
    type Prop InputRef (SearchInput ms) = JSV -> Ef ms IO ()
    getProp _ = inputRef
    setProp _ ir dsi = dsi { inputRef = ir }

instance HasProp OnChange (SearchInput ms) where
    type Prop OnChange (SearchInput ms) = Txt -> Ef ms IO ()
    getProp _ = onChange
    setProp _ oc dsi = dsi { onChange = oc }

instance HasProp TabIndex (SearchInput ms) where
    type Prop TabIndex (SearchInput ms) = Maybe Int
    getProp _ = tabIndex
    setProp _ ti dsi = dsi { tabIndex = ti }

instance HasProp Type (SearchInput ms) where
    type Prop Type (SearchInput ms) = Txt
    getProp _ = _type
    setProp _ t dsi = dsi { _type = t }

instance HasProp Value (SearchInput ms) where
    type Prop Value (SearchInput ms) = Txt
    getProp _ = value
    setProp _ v dsi = dsi { value = v }
