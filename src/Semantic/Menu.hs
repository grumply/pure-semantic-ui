{-# LANGUAGE UndecidableInstances #-}
module Semantic.Menu
  ( module Properties
  , module Tools
  , Menu(..), pattern Semantic.Menu.Menu
  , Header(..), pattern Semantic.Menu.Header
  , Item(..), pattern Item
  , Submenu (..), pattern Submenu
  ) where

import Pure hiding (Icon,color,fixed,position,text,vertical,index,size,active,link,disabled,(#))

import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
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

data Menu = Menu_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , attached :: Maybe Txt
    , borderless :: Bool
    , color :: Txt
    , compact :: Bool
    , fixed :: Txt
    , floated :: Maybe Txt
    , fluid :: Bool
    , icon :: Maybe Txt
    , inverted :: Bool
    , onItemClick :: Item -> IO ()
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

instance Default Menu where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Menu :: Menu -> Menu
pattern Menu m = m

instance Pure Menu where
    view Menu_ {..} =
        let
            cs =
                [ "ui"
                , color
                , size
                , borderless # "borderless"
                , compact # "compact"
                , fluid # "fluid"
                , inverted # "inverted"
                , pagination # "pagination"
                , pointing # "pointing"
                , secondary # "secondary"
                , stackable # "stackable"
                , text # "text"
                , vertical # "vertical"
                , maybe "" (<>> "attached") attached
                , maybe "" (<>> "floated") floated
                , maybe "" (<>> "icon") icon
                , maybe "" (<>> "tabular") tabular
                , (fixed /= mempty) # (fixed <>> "fixed")
                , widthProp widths "item" def
                , "menu"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Menu where
    type Prop As Menu = Features -> [View] -> View
    getProp _ = as
    setProp _ a m = m { as = a }

instance HasFeatures Menu where
    getFeatures = features
    setFeatures as m = m { features = as }

instance HasChildren Menu where
    getChildren = children
    setChildren cs m = m { children = cs }

instance HasProp Attached Menu where
    type Prop Attached Menu = Maybe Txt
    getProp _ = attached
    setProp _ a m = m { attached = a }

instance HasProp Borderless Menu where
    type Prop Borderless Menu = Bool
    getProp _ = borderless
    setProp _ b m = m { borderless = b }

instance HasProp Color Menu where
    type Prop Color Menu = Txt
    getProp _ = color
    setProp _ c m = m { color = c }

instance HasProp Compact Menu where
    type Prop Compact Menu = Bool
    getProp _ = compact
    setProp _ c m = m { compact = c }

instance HasProp Fixed Menu where
    type Prop Fixed Menu = Txt
    getProp _ = fixed
    setProp _ f m = m { fixed = f }

instance HasProp Floated Menu where
    type Prop Floated Menu = Maybe Txt
    getProp _ = floated
    setProp _ f m = m { floated = f }

instance HasProp Fluid Menu where
    type Prop Fluid Menu = Bool
    getProp _ = fluid
    setProp _ f m = m { fluid = f }

instance HasProp IsIcon Menu where
    type Prop IsIcon Menu = Maybe Txt
    getProp _ = icon
    setProp _ i m = m { icon = i }

instance HasProp Inverted Menu where
    type Prop Inverted Menu = Bool
    getProp _ = inverted
    setProp _ i m = m { inverted = i }

instance HasProp Pagination Menu where
    type Prop Pagination Menu = Bool
    getProp _ = pagination
    setProp _ p m = m { pagination = p }

instance HasProp Pointing Menu where
    type Prop Pointing Menu = Bool
    getProp _ = pointing
    setProp _ p m = m { pointing = p }

instance HasProp Secondary Menu where
    type Prop Secondary Menu = Bool
    getProp _ = secondary
    setProp _ s m = m { secondary = s }

instance HasProp Size Menu where
    type Prop Size Menu = Txt
    getProp _ = size
    setProp _ s m = m { size = s }

instance HasProp Stackable Menu where
    type Prop Stackable Menu = Bool
    getProp _ = stackable
    setProp _ s m = m { stackable = s }

instance HasProp Tabular Menu where
    type Prop Tabular Menu = Maybe Txt
    getProp _ = tabular
    setProp _ t m = m { tabular = t }

instance HasProp IsText Menu where
    type Prop IsText Menu = Bool
    getProp _ = text
    setProp _ t m = m { text = t }

instance HasProp Vertical Menu where
    type Prop Vertical Menu = Bool
    getProp _ = vertical
    setProp _ v m = m { vertical = v }

instance HasProp Widths Menu where
    type Prop Widths Menu = Txt
    getProp _ = widths
    setProp _ w m = m { widths = w }

data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header mh = mh

instance Pure Header where
    view Header_ {..} = as (features & Class "header") children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ a mh = mh { as = a }

instance HasFeatures Header where
    getFeatures = features
    setFeatures as mh = mh { features = as }

instance HasChildren Header where
    getChildren = children
    setChildren cs mh = mh { children = cs }

data Item = Item_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , color :: Txt
    , disabled :: Bool
    , fitted :: Maybe Txt
    , header :: Bool
    , index :: Int
    , link :: Bool
    , position :: Txt
    } deriving (Generic)

instance Default Item where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Item :: Item -> Item
pattern Item mi = mi

instance Pure Item where
    view Item_ {..} =
        let
            icon =
                case children of
                    [ View (Icon i) ] -> True
                    _                 -> False

            cs =
                [ color
                , position
                , active # "active"
                , disabled # "disabled"
                , icon # "icon"
                , header # "header"
                , link # "link"
                , maybe "" ("fitted" <<>>) fitted
                , "item"
                ]

        in
            as (features & Classes cs) children

instance HasProp As Item where
    type Prop As Item = Features -> [View] -> View
    getProp _ = as
    setProp _ a mi = mi { as = a }

instance HasFeatures Item where
    getFeatures = features
    setFeatures as mi = mi { features = as }

instance HasChildren Item where
    getChildren = children
    setChildren cs mi = mi { children = cs }

instance HasProp Active Item where
    type Prop Active Item = Bool
    getProp _ = active
    setProp _ a mi = mi { active = a }

instance HasProp Color Item where
    type Prop Color Item = Txt
    getProp _ = color
    setProp _ c mi = mi { color = c }

instance HasProp Disabled Item where
    type Prop Disabled Item = Bool
    getProp _ = disabled
    setProp _ d mi = mi { disabled = d }

instance HasProp Fitted Item where
    type Prop Fitted Item = Maybe Txt
    getProp _ = fitted
    setProp _ f mi = mi { fitted = f }

instance HasProp IsHeader Item where
    type Prop IsHeader Item = Bool
    getProp _ = header
    setProp _ h mi = mi { header = h }

instance HasProp Index Item where
    type Prop Index Item = Int
    getProp _ = index
    setProp _ i mi = mi { index = i }

instance HasProp Link Item where
    type Prop Link Item = Bool
    getProp _ = link
    setProp _ l mi = mi { link = l }

instance HasProp Position Item where
    type Prop Position Item = Txt
    getProp _ = position
    setProp _ p mi = mi { position = p }

data Submenu = Submenu_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , position :: Txt
    } deriving (Generic)

instance Default Submenu where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Submenu :: Submenu -> Submenu
pattern Submenu mm = mm

instance Pure Submenu where
    view Submenu_ {..} = as (features & Classes [ position, "menu" ]) children

instance HasProp As Submenu where
    type Prop As Submenu = Features -> [View] -> View
    getProp _ = as
    setProp _ a mm = mm { as = a }

instance HasFeatures Submenu where
    getFeatures = features
    setFeatures as mm = mm { features = as }

instance HasChildren Submenu where
    getChildren = children
    setChildren cs mm = mm { children = cs }

instance HasProp Position Submenu where
    type Prop Position Submenu = Txt
    getProp _ = position
    setProp _ p mm = mm { position = p }
