module Semantic.Label
  ( module Properties
  , module Tools
  , Label(..), pattern Label
  , Detail(..), pattern Detail
  , Group(..), pattern Group
  ) where

import GHC.Generics as G (Generic,to)
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML

import Semantic.Utils

import Semantic.Image (Image(..))

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Basic, Basic(..)
  , pattern Circular, Circular(..)
  , pattern Color, Color(..)
  , pattern Corner, Corner(..)
  , pattern Empty, Empty(..)
  , pattern Floating, Floating(..)
  , pattern Horizontal, Horizontal(..)
  , pattern Pointing, Pointing(..)
  , pattern Ribbon, Ribbon(..)
  , pattern Size, Size(..)
  , pattern Tag, Tag(..)
  )

import Prelude hiding (Floating)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Label = Label_
    { as :: Features -> [View] -> View
    , active :: Bool
    , attached :: Txt
    , features :: Features
    , basic :: Bool
    , children :: [View]
    , circular :: Bool
    , color :: Txt
    , corner :: Maybe Txt
    , empty :: Bool
    , floating :: Bool
    , horizontal :: Bool
    , pointing :: Maybe Txt
    , ribbon :: Maybe Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default Label where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Label :: Label -> Label
pattern Label l = l

instance Pure Label where
    view Label_ {..} =
        let
            pointingClass =
                -- note the careful class ordering on the RHS of the cases;
                -- sui-css uses [class * ="left pointing"]/[class * ="pointing below"]
                ($ "pointing") $ flip (maybe (const def)) pointing $ \case
                    (oneEq "left" "right"  -> Just lr) -> (lr <<>>)
                    (oneEq "above" "below" -> Just ab) -> (<<>> ab)
                    "" -> id
                    _  -> const def

            hasImage = not $ List.null $ List.filter isImage children
              where
                isImage (View Image_ {}) = True
                isImage _ = False

            cs =
                [ "ui"
                , color
                , pointingClass
                , size
                , active # "active"
                , basic # "basic"
                , circular # "circular"
                , empty # "empty"
                , floating # "floating"
                , horizontal # "horizontal"
                , hasImage # "image"
                , tag # "tag"
                , corner # "corner"
                , ribbon # "ribbon"
                , attached # "attached"
                , "label"
                ]
        in

instance HasProp Active Label where
    type Prop Active Label = Bool
    getProp _ = active
    setProp _ a l = l { active = a }

instance HasProp As Label where
    type Prop As Label = Features -> [View] -> View
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasProp Attached Label where
    type Prop Attached Label = Txt
    getProp _ = attached
    setProp _ attach l = l { attached = attach }

instance HasFeatures Label where
    type Prop Attributes Label = Features
    getProp _ = attributes
    setProp _ cs l = l { attributes = cs }

instance HasProp Basic Label where
    type Prop Basic Label = Bool
    getProp _ = basic
    setProp _ b l = l { basic = b }

instance HasChildren Label where
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasProp Circular Label where
    type Prop Circular Label = Bool
    getProp _ = circular
    setProp _ c l = l { circular = c }

instance HasProp OnClick Label where
    type Prop OnClick Label = IO ()
    getProp _ = onClick
    setProp _ oc l = l { onClick = oc }

instance HasProp Color Label where
    type Prop Color Label = Txt
    getProp _ = color
    setProp _ c l = l { color = c }

instance HasProp Corner Label where
    type Prop Corner Label = Maybe Txt
    getProp _ = corner
    setProp _ c l = l { corner = c }

instance HasProp Empty Label where
    type Prop Empty Label = Bool
    getProp _ = empty
    setProp _ e l = l { empty = e }

instance HasProp Floating Label where
    type Prop Floating Label = Bool
    getProp _ = floating
    setProp _ f l = l { floating = f }

instance HasProp Horizontal Label where
    type Prop Horizontal Label = Bool
    getProp _ = horizontal
    setProp _ h l = l { horizontal = h }

instance HasProp Pointing Label where
    type Prop Pointing Label = Maybe Txt
    getProp _ = pointing
    setProp _ p l = l { pointing = p }

instance HasProp Ribbon Label where
    type Prop Ribbon Label = Maybe Txt
    getProp _ = ribbon
    setProp _ r l = l { ribbon = r }

instance HasProp Size Label where
    type Prop Size Label = Txt
    getProp _ = size
    setProp _ s l = l { size = s }

instance HasProp Tag Label where
    type Prop Tag Label = Bool
    getProp _ = tag
    setProp _ t l = l { tag = t }

data Detail = Detail_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Detail where
    def = G.to gdef

pattern Detail :: Detail -> Detail
pattern Detail ld = ld

instance Pure Detail where
    view Detail_ {..} =
        as
            : attributes
            )
            children

instance HasProp As Detail where
    type Prop As Detail = Features -> [View] -> View
    getProp _ = as
    setProp _ f ld = ld { as = f }

instance HasFeatures Detail where
    getFeatures = features
    setFeatures cs ld = ld { features = cs }

instance HasChildren Detail where
    getChildren = children
    setChildren cs ld = ld { children = cs }

data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , circular :: Bool
    , color :: Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = Div }

pattern Group :: Group -> Group
pattern Group lg = lg

instance Pure Group where
    view Group_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : circular # "circular"
                : tag # "tag"
                : "labels"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ f lg = lg { as = f }

instance HasFeatures Group where
    getFeatures = features
    setFeatures cs lg = lg { features = cs }

instance HasChildren Group where
    getChildren = children
    setChildren cs lg = lg { children = cs }

instance HasProp Circular Group where
    type Prop Circular Group = Bool
    getProp _ = circular
    setProp _ c lg = lg { circular = c }

instance HasProp Color Group where
    type Prop Color Group = Txt
    getProp _ = color
    setProp _ c lg = lg { color = c }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ s lg = lg { size = s }

instance HasProp Tag Group where
    type Prop Tag Group = Bool
    getProp _ = tag
    setProp _ t lg = lg { tag = t }
