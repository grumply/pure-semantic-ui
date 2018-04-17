{-# LANGUAGE UndecidableInstances #-}
module Semantic.Menu
  ( module Properties
  , module Tools
  , Menu(..), pattern Menu
  , Header(..), pattern Header
  , Item(..), pattern Item
  , Submenu (..), pattern Submenu
  ) where

import GHC.Generics as G
import Pure.View hiding (active,color,fixed,onClick,text,vertical,widths,position,disabled,Menu,Header)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Attached, Attached(..)
  , pattern Borderless, Borderless(..)
  , pattern Color, Color(..)
  , pattern Compact, Compact(..)
  , pattern Fixed, Fixed(..)
  , pattern Floated, Floated(..)
  , pattern Fluid, Fluid(..)
  , pattern Inverted, Inverted(..)
  , pattern IsIcon, IsIcon(..)
  , pattern IsText, IsText(..)
  , pattern OnClick, OnClick(..)
  , pattern Pagination, Pagination(..)
  , pattern Pointing, Pointing(..)
  , pattern Secondary, Secondary(..)
  , pattern Size, Size(..)
  , pattern Stackable, Stackable(..)
  , pattern Tabular, Tabular(..)
  , pattern Vertical, Vertical(..)
  , pattern Widths, Widths(..)
  , pattern Active, Active(..)
  , pattern Disabled, Disabled(..)
  , pattern Fitted, Fitted(..)
  , pattern Index, Index(..)
  , pattern IsHeader, IsHeader(..)
  , pattern Link, Link(..)
  , pattern Position, Position(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Semantic.Icon

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Menu ms = Menu_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , borderless :: Bool
    , color :: Txt
    , compact :: Bool
    , fixed :: Txt
    , floated :: Maybe Txt
    , fluid :: Bool
    , icon :: Maybe Txt
    , inverted :: Bool
    , onItemClick :: Item ms -> Ef ms IO ()
    , pagination :: Bool
    , pointing :: Bool
    , secondary :: Bool
    , size :: Txt
    , stackable :: Bool
    , tabular :: Maybe Txt
    , text :: Bool
    , vertical :: Bool
    , widths :: Txt
    } deriving (Generic)

instance Default (Menu ms) where
    def = (G.to gdef) { as = Div }

pattern Menu :: VC ms => Menu ms -> View ms
pattern Menu m = View m

instance VC ms => Pure Menu ms where
    render Menu_ {..} =
        let
            children' =
                mapPures (\mi@(Item_ {}) -> mi { onClick = onClick mi >> onItemClick mi }) children

            cs =
                ( "ui"
                : color
                : size
                : borderless # "borderless"
                : compact # "compact"
                : fluid # "fluid"
                : inverted # "inverted"
                : pagination # "pagination"
                : pointing # "pointing"
                : secondary # "secondary"
                : stackable # "stackable"
                : text # "text"
                : vertical # "vertical"
                : may (<>> "attached") attached
                : may (<>> "floated") floated
                : may (<>> "icon") icon
                : may (<>> "tabular") tabular
                : fixed # (fixed <>> "fixed")
                : widthProp widths "item" def
                : "menu"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children'

instance HasProp As (Menu ms) where
    type Prop As (Menu ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a m = m { as = a }

instance HasProp Attributes (Menu ms) where
    type Prop Attributes (Menu ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as m = m { attributes = as }

instance HasProp Children (Menu ms) where
    type Prop Children (Menu ms) = [View ms]
    getProp _ = children
    setProp _ cs m = m { children = cs }

instance HasProp Classes (Menu ms) where
    type Prop Classes (Menu ms) = [Txt]
    getProp _ = classes
    setProp _ cs m = m { classes = cs }

instance HasProp Attached (Menu ms) where
    type Prop Attached (Menu ms) = Maybe Txt
    getProp _ = attached
    setProp _ a m = m { attached = a }

instance HasProp Borderless (Menu ms) where
    type Prop Borderless (Menu ms) = Bool
    getProp _ = borderless
    setProp _ b m = m { borderless = b }

instance HasProp Color (Menu ms) where
    type Prop Color (Menu ms) = Txt
    getProp _ = color
    setProp _ c m = m { color = c }

instance HasProp Compact (Menu ms) where
    type Prop Compact (Menu ms) = Bool
    getProp _ = compact
    setProp _ c m = m { compact = c }

instance HasProp Fixed (Menu ms) where
    type Prop Fixed (Menu ms) = Txt
    getProp _ = fixed
    setProp _ f m = m { fixed = f }

instance HasProp Floated (Menu ms) where
    type Prop Floated (Menu ms) = Maybe Txt
    getProp _ = floated
    setProp _ f m = m { floated = f }

instance HasProp Fluid (Menu ms) where
    type Prop Fluid (Menu ms) = Bool
    getProp _ = fluid
    setProp _ f m = m { fluid = f }

instance HasProp IsIcon (Menu ms) where
    type Prop IsIcon (Menu ms) = Maybe Txt
    getProp _ = icon
    setProp _ i m = m { icon = i }

instance HasProp Inverted (Menu ms) where
    type Prop Inverted (Menu ms) = Bool
    getProp _ = inverted
    setProp _ i m = m { inverted = i }

instance HasProp OnClick (Menu ms) where
    type Prop OnClick (Menu ms) = Item ms -> Ef ms IO ()
    getProp _ = onItemClick
    setProp _ oc m = m { onItemClick = oc }

instance HasProp Pagination (Menu ms) where
    type Prop Pagination (Menu ms) = Bool
    getProp _ = pagination
    setProp _ p m = m { pagination = p }

instance HasProp Pointing (Menu ms) where
    type Prop Pointing (Menu ms) = Bool
    getProp _ = pointing
    setProp _ p m = m { pointing = p }

instance HasProp Secondary (Menu ms) where
    type Prop Secondary (Menu ms) = Bool
    getProp _ = secondary
    setProp _ s m = m { secondary = s }

instance HasProp Size (Menu ms) where
    type Prop Size (Menu ms) = Txt
    getProp _ = size
    setProp _ s m = m { size = s }

instance HasProp Stackable (Menu ms) where
    type Prop Stackable (Menu ms) = Bool
    getProp _ = stackable
    setProp _ s m = m { stackable = s }

instance HasProp Tabular (Menu ms) where
    type Prop Tabular (Menu ms) = Maybe Txt
    getProp _ = tabular
    setProp _ t m = m { tabular = t }

instance HasProp IsText (Menu ms) where
    type Prop IsText (Menu ms) = Bool
    getProp _ = text
    setProp _ t m = m { text = t }

instance HasProp Vertical (Menu ms) where
    type Prop Vertical (Menu ms) = Bool
    getProp _ = vertical
    setProp _ v m = m { vertical = v }

instance HasProp Widths (Menu ms) where
    type Prop Widths (Menu ms) = Txt
    getProp _ = widths
    setProp _ w m = m { widths = w }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header mh = View mh

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
    setProp _ a mh = mh { as = a }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as mh = mh { attributes = as }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs mh = mh { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs mh = mh { classes = cs }

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , color :: Txt
    , disabled :: Bool
    , fitted :: Maybe Txt
    , header :: Bool
    , index :: Int
    , link :: Bool
    , onClick :: Ef ms IO ()
    , position :: Txt
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Item ms -> View ms
pattern Item mi = View mi

instance Pure Item ms where
    render Item_ {..} =
        let
            e = onClick ? A $ as

            icon =
                case children of
                    [ Icon i ] -> True
                    _          -> False

            cs =
                ( color
                : position
                : active # "active"
                : disabled # "disabled"
                : icon # "icon"
                : header # "header"
                : link # "link"
                : may ("fitted" <<>>) fitted
                : "item"
                : classes
                )

        in
            e
                ( mergeClasses $ ClassList cs
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasProp As (Item ms) where
    type Prop As (Item ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a mi = mi { as = a }

instance HasProp Attributes (Item ms) where
    type Prop Attributes (Item ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as mi = mi { attributes = as }

instance HasProp Children (Item ms) where
    type Prop Children (Item ms) = [View ms]
    getProp _ = children
    setProp _ cs mi = mi { children = cs }

instance HasProp Classes (Item ms) where
    type Prop Classes (Item ms) = [Txt]
    getProp _ = classes
    setProp _ cs mi = mi { classes = cs }

instance HasProp Active (Item ms) where
    type Prop Active (Item ms) = Bool
    getProp _ = active
    setProp _ a mi = mi { active = a }

instance HasProp Color (Item ms) where
    type Prop Color (Item ms) = Txt
    getProp _ = color
    setProp _ c mi = mi { color = c }

instance HasProp Disabled (Item ms) where
    type Prop Disabled (Item ms) = Bool
    getProp _ = disabled
    setProp _ d mi = mi { disabled = d }

instance HasProp Fitted (Item ms) where
    type Prop Fitted (Item ms) = Maybe Txt
    getProp _ = fitted
    setProp _ f mi = mi { fitted = f }

instance HasProp IsHeader (Item ms) where
    type Prop IsHeader (Item ms) = Bool
    getProp _ = header
    setProp _ h mi = mi { header = h }

instance HasProp Index (Item ms) where
    type Prop Index (Item ms) = Int
    getProp _ = index
    setProp _ i mi = mi { index = i }

instance HasProp Link (Item ms) where
    type Prop Link (Item ms) = Bool
    getProp _ = link
    setProp _ l mi = mi { link = l }

instance HasProp OnClick (Item ms) where
    type Prop OnClick (Item ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc mi = mi { onClick = oc }

instance HasProp Position (Item ms) where
    type Prop Position (Item ms) = Txt
    getProp _ = position
    setProp _ p mi = mi { position = p }

data Submenu ms = Submenu_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , position :: Txt
    } deriving (Generic)

instance Default (Submenu ms) where
    def = (G.to gdef) { as = Div }

pattern Submenu :: Submenu ms -> View ms
pattern Submenu mm = View mm

instance Pure Submenu ms where
    render Submenu_ {..} =
        let
            cs =
                ( position
                : "menu"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Submenu ms) where
    type Prop As (Submenu ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a mm = mm { as = a }

instance HasProp Attributes (Submenu ms) where
    type Prop Attributes (Submenu ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as mm = mm { attributes = as }

instance HasProp Children (Submenu ms) where
    type Prop Children (Submenu ms) = [View ms]
    getProp _ = children
    setProp _ cs mm = mm { children = cs }

instance HasProp Classes (Submenu ms) where
    type Prop Classes (Submenu ms) = [Txt]
    getProp _ = classes
    setProp _ cs mm = mm { classes = cs }

instance HasProp Position (Submenu ms) where
    type Prop Position (Submenu ms) = Txt
    getProp _ = position
    setProp _ p mm = mm { position = p }
