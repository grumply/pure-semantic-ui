module Semantic.Dropdown
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

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
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

data Dropdown = Dropdown_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
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
    , open :: Bool
    , pointing :: Maybe Txt
    , search :: Bool
    , selection :: Bool
    , simple :: Bool
    , scrolling :: Bool
    , tabIndex :: Maybe Int
    , upward :: Bool
    } deriving (Generic)

instance Default Dropdown where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Dropdown :: Dropdown -> Dropdown
pattern Dropdown d = d

instance Pure Dropdown where
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
                )
        in as
                : may Tabindex tabIndex
                : attributes
                )
                children

instance HasProp As Dropdown where
    type Prop As Dropdown = Features -> [View] -> View
    getProp _ = as
    setProp _ f dd = dd { as = f }

instance HasFeatures Dropdown where
    getFeatures = features
    setFeatures cs dd = dd { features = cs }

instance HasChildren Dropdown where
    getChildren = children
    setChildren cs dd = dd { children = cs }


instance HasProp Basic Dropdown where
    type Prop Basic Dropdown = Bool
    getProp _ = basic
    setProp _ b dd = dd { basic = b }

instance HasProp IsButton Dropdown where
    type Prop IsButton Dropdown = Bool
    getProp _ = button
    setProp _ b dd = dd { button = b }

instance HasProp Compact Dropdown where
    type Prop Compact Dropdown = Bool
    getProp _ = compact
    setProp _ c dd = dd { compact = c }

instance HasProp Disabled Dropdown where
    type Prop Disabled Dropdown = Bool
    getProp _ = disabled
    setProp _ d dd = dd { disabled = d }

instance HasProp Error Dropdown where
    type Prop Error Dropdown = Bool
    getProp _ = error
    setProp _ e dd = dd { error = e }

instance HasProp Fluid Dropdown where
    type Prop Fluid Dropdown = Bool
    getProp _ = fluid
    setProp _ f dd = dd { fluid = f }

instance HasProp Floating Dropdown where
    type Prop Floating Dropdown = Bool
    getProp _ = floating
    setProp _ f dd = dd { floating = f }

instance HasProp Inline Dropdown where
    type Prop Inline Dropdown = Bool
    getProp _ = inline
    setProp _ i dd = dd { inline = i }

instance HasProp Labeled Dropdown where
    type Prop Labeled Dropdown = Bool
    getProp _ = labeled
    setProp _ l dd = dd { labeled = l }

instance HasProp Loading Dropdown where
    type Prop Loading Dropdown = Bool
    getProp _ = loading
    setProp _ l dd = dd { loading = l }

instance HasProp IsItem Dropdown where
    type Prop IsItem Dropdown = Bool
    getProp _ = item
    setProp _ i dd = dd { item = i }

instance HasProp Multiple Dropdown where
    type Prop Multiple Dropdown = Bool
    getProp _ = multiple
    setProp _ m dd = dd { multiple = m }

instance HasProp Open Dropdown where
    type Prop Open Dropdown = Bool
    getProp _ = open
    setProp _ o dd = dd { open = o }

instance HasProp Pointing Dropdown where
    type Prop Pointing Dropdown = Maybe Txt
    getProp _ = pointing
    setProp _ p dd = dd { pointing = p }

instance HasProp IsSearch Dropdown where
    type Prop IsSearch Dropdown = Bool
    getProp _ = search
    setProp _ is dd = dd { search = is }

instance HasProp Selection Dropdown where
    type Prop Selection Dropdown = Bool
    getProp _ = selection
    setProp _ s dd = dd { selection = s }

instance HasProp Simple Dropdown where
    type Prop Simple Dropdown = Bool
    getProp _ = simple
    setProp _ s dd = dd { simple = s }

instance HasProp Scrolling Dropdown where
    type Prop Scrolling Dropdown = Bool
    getProp _ = scrolling
    setProp _ s dd = dd { scrolling = s }

instance HasProp TabIndex Dropdown where
    type Prop TabIndex Dropdown = Maybe Int
    getProp _ = tabIndex
    setProp _ ti dd = dd { tabIndex = ti }

instance HasProp Upward Dropdown where
    type Prop Upward Dropdown = Bool
    getProp _ = upward
    setProp _ u dd = dd { upward = u }

data Divider = Divider_
    { as :: Features -> [View] -> View
    , features :: Features
    } deriving (Generic)

instance Default Divider where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Divider :: Divider -> Divider
pattern Divider dd = dd

instance Pure Divider where
    render Divider_ {..} =
        let
            cs =
                ( "divider"
                )
        in
            as
                : attributes
                )
                []

instance HasProp As Divider where
    type Prop As Divider = Features -> [View] -> View
    getProp _ = as
    setProp _ f dd = dd { as = f }

instance HasFeatures Divider where
    getFeatures = features
    setFeatures cs dd = dd { features = cs }


data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header dh = dh

instance Pure Header where
    render Header_ {..} =
        let
            cs =
                ( "header"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ f dh = dh { as = f }

instance HasFeatures Header where
    getFeatures = features
    setFeatures cs dh = dh { features = cs }

instance HasChildren Header where
    getChildren = children
    setChildren cs dh = dh { children = cs }


data Item = Item_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , disabled :: Bool
    , selected :: Bool
    } deriving (Generic)

instance Default Item where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Item :: Item -> Item
pattern Item di = di

instance Pure (Item ) where
    render di@Item_ {..} =
        let
            cs =
                ( active # "active"
                : disabled # "disabled"
                : selected # "selected"
                : "item"
                )
        in
            as
                : Role "option"
                : attributes
                )
                children

instance HasProp Active Item where
    type Prop Active Item = Bool
    getProp _ = active
    setProp _ a di = di { active = a }

instance HasProp As Item where
    type Prop As Item = Features -> [View] -> View
    getProp _ = as
    setProp _ f di = di { as = f }

instance HasFeatures Item where
    getFeatures = features
    setFeatures cs di = di { features = cs }

instance HasChildren Item where
    getChildren = children
    setChildren cs di = di { children = cs }


instance HasProp Disabled Item where
    type Prop Disabled Item = Bool
    getProp _ = disabled
    setProp _ d di = di { disabled = d }

instance HasProp Selected Item where
    type Prop Selected Item = Bool
    getProp _ = selected
    setProp _ s di = di { selected = s }

data Menu = Menu_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , scrolling :: Bool
    } deriving (Generic)

instance Default Menu where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Menu :: Menu -> Menu
pattern Menu dm = dm

instance Pure Menu where
    render Menu_ {..} =
        let
            cs =
                ( scrolling # "scrolling"
                : "menu transition"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Menu where
    type Prop As Menu = Features -> [View] -> View
    getProp _ = as
    setProp _ f dm = dm { as = f }

instance HasFeatures Menu where
    getFeatures = features
    setFeatures cs dm = dm { features = cs }

instance HasChildren Menu where
    getChildren = children
    setChildren cs dm = dm { children = cs }


instance HasProp Scrolling Menu where
    type Prop Scrolling Menu = Bool
    getProp _ = scrolling
    setProp _ s dm = dm { scrolling = s }

data SearchInput = SearchInput_
    { features :: Features
    , inputRef :: JSV -> IO ()
    , tabIndex :: Maybe Int
    , _type :: Txt
    , value :: Txt
    } deriving (Generic)

instance Default SearchInput where
    def = (G.to gdef) { _type = "text" }

pattern SearchInput :: SearchInput -> SearchInput
pattern SearchInput dh = dh

instance Pure SearchInput where
    render SearchInput_ {..} =
        let
            cs =
                ( "search"
                )
        in
            Input
                ( HTML.Value value
                : HostRef (\(Node n) -> return . Just $ inputRef n)
                : may Tabindex tabIndex
                : HTML.Type _type
                : Attribute "autoComplete" "off"
                )
                []

instance HasFeatures SearchInput where
    getFeatures = features
    setFeatures cs dsi = dsi { features = cs }

instance HasProp InputRef SearchInput where
    type Prop InputRef SearchInput = JSV -> IO ()
    getProp _ = inputRef
    setProp _ ir dsi = dsi { inputRef = ir }

instance HasProp TabIndex SearchInput where
    type Prop TabIndex SearchInput = Maybe Int
    getProp _ = tabIndex
    setProp _ ti dsi = dsi { tabIndex = ti }

instance HasProp Type SearchInput where
    type Prop Type SearchInput = Txt
    getProp _ = _type
    setProp _ t dsi = dsi { _type = t }

instance HasProp Value SearchInput where
    type Prop Value SearchInput = Txt
    getProp _ = value
    setProp _ v dsi = dsi { value = v }
