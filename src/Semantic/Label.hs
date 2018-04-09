module Semantic.Label
  ( module Properties
  , module Tools
  , Label(..), pattern Label
  , Detail(..), pattern Detail
  , Group(..), pattern Group
  ) where

import GHC.Generics as G
import Pure.View as View hiding (active,color,empty,horizontal,onClick,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Image (Image(..))

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Attributes, Attributes(..)
  , pattern Basic, Basic(..)
  , pattern Children, Children(..)
  , pattern Circular, Circular(..)
  , pattern Classes, Classes(..)
  , pattern Color, Color(..)
  , pattern Corner, Corner(..)
  , pattern Empty, Empty(..)
  , pattern Floating, Floating(..)
  , pattern Horizontal, Horizontal(..)
  , pattern OnClick, OnClick(..)
  , pattern Pointing, Pointing(..)
  , pattern Ribbon, Ribbon(..)
  , pattern Size, Size(..)
  , pattern Tag, Tag(..)
  )

import Prelude hiding (Floating)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Label ms = Label_
    { as :: [Feature ms] -> [View ms] -> View ms
    , active :: Bool
    , attached :: Txt
    , attributes :: [Feature ms]
    , basic :: Bool
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , corner :: Maybe Txt
    , empty :: Bool
    , floating :: Bool
    , horizontal :: Bool
    , onClick :: Ef ms IO ()
    , pointing :: Maybe Txt
    , ribbon :: Maybe Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (Label ms) where
    def = (G.to gdef) { as = Div }

pattern Label :: Label ms -> View ms
pattern Label l = View l

instance Pure Label ms where
    render Label_ {..} =
        let
            pointingClass =
                -- note the careful class ordering
                ($ "pointing") $ flip (maybe (const nil)) pointing $ \case
                    (oneEq "left" "right"  -> Just lr) -> (lr <<>>)
                    (oneEq "above" "below" -> Just ab) -> (<<>> ab)
                    "" -> id
                    _  -> const nil

            hasImage =
                foldPures (\(Image_ {}) -> const True) False children

            cs =
                ( "ui"
                : color
                : pointingClass
                : size
                : active # "active"
                : basic # "basic"
                : circular # "circular"
                : empty # "empty"
                : floating # "floating"
                : horizontal # "horizontal"
                : hasImage # "image"
                : tag # "tag"
                : corner # "corner"
                : ribbon # "ribbon"
                : attached # "attached"
                : "label"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : onClick # HTML.onClick onClick
                : attributes
                )
                children

instance HasProp Active (Label ms) where
    type Prop Active (Label ms) = Bool
    getProp _ = active
    setProp _ a l = l { active = a }

instance HasProp As (Label ms) where
    type Prop As (Label ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasProp Attached (Label ms) where
    type Prop Attached (Label ms) = Txt
    getProp _ = attached
    setProp _ attach l = l { attached = attach }

instance HasProp Attributes (Label ms) where
    type Prop Attributes (Label ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs l = l { attributes = cs }

instance HasProp Basic (Label ms) where
    type Prop Basic (Label ms) = Bool
    getProp _ = basic
    setProp _ b l = l { basic = b }

instance HasProp Children (Label ms) where
    type Prop Children (Label ms) = [View ms]
    getProp _ = children
    setProp _ cs l = l { children = cs }

instance HasProp Circular (Label ms) where
    type Prop Circular (Label ms) = Bool
    getProp _ = circular
    setProp _ c l = l { circular = c }

instance HasProp Classes (Label ms) where
    type Prop Classes (Label ms) = [Txt]
    getProp _ = classes
    setProp _ cs l = l { classes = cs }

instance HasProp OnClick (Label ms) where
    type Prop OnClick (Label ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc l = l { onClick = oc }

instance HasProp Color (Label ms) where
    type Prop Color (Label ms) = Txt
    getProp _ = color
    setProp _ c l = l { color = c }

instance HasProp Corner (Label ms) where
    type Prop Corner (Label ms) = Maybe Txt
    getProp _ = corner
    setProp _ c l = l { corner = c }

instance HasProp Empty (Label ms) where
    type Prop Empty (Label ms) = Bool
    getProp _ = empty
    setProp _ e l = l { empty = e }

instance HasProp Floating (Label ms) where
    type Prop Floating (Label ms) = Bool
    getProp _ = floating
    setProp _ f l = l { floating = f }

instance HasProp Horizontal (Label ms) where
    type Prop Horizontal (Label ms) = Bool
    getProp _ = horizontal
    setProp _ h l = l { horizontal = h }

instance HasProp Pointing (Label ms) where
    type Prop Pointing (Label ms) = Maybe Txt
    getProp _ = pointing
    setProp _ p l = l { pointing = p }

instance HasProp Ribbon (Label ms) where
    type Prop Ribbon (Label ms) = Maybe Txt
    getProp _ = ribbon
    setProp _ r l = l { ribbon = r }

instance HasProp Size (Label ms) where
    type Prop Size (Label ms) = Txt
    getProp _ = size
    setProp _ s l = l { size = s }

instance HasProp Tag (Label ms) where
    type Prop Tag (Label ms) = Bool
    getProp _ = tag
    setProp _ t l = l { tag = t }

data Detail ms = Detail_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Detail ms) where
    def = G.to gdef

pattern Detail :: Detail ms -> View ms
pattern Detail ld = View ld

instance Pure Detail ms where
    render Detail_ {..} =
        as
            ( ClassList ("detail" : classes)
            : attributes
            )
            children

instance HasProp As (Detail ms) where
    type Prop As (Detail ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f ld = ld { as = f }

instance HasProp Attributes (Detail ms) where
    type Prop Attributes (Detail ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs ld = ld { attributes = cs }

instance HasProp Children (Detail ms) where
    type Prop Children (Detail ms) = [View ms]
    getProp _ = children
    setProp _ cs ld = ld { children = cs }

instance HasProp Classes (Detail ms) where
    type Prop Classes (Detail ms) = [Txt]
    getProp _ = classes
    setProp _ cs ld = ld { classes = cs }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group lg = View lg

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : circular # "circular"
                : tag # "tag"
                : "labels"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Group ms) where
    type Prop As (Group ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f lg = lg { as = f }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs lg = lg { attributes = cs }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs lg = lg { children = cs }

instance HasProp Circular (Group ms) where
    type Prop Circular (Group ms) = Bool
    getProp _ = circular
    setProp _ c lg = lg { circular = c }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs lg = lg { classes = cs }

instance HasProp Color (Group ms) where
    type Prop Color (Group ms) = Txt
    getProp _ = color
    setProp _ c lg = lg { color = c }

instance HasProp Size (Group ms) where
    type Prop Size (Group ms) = Txt
    getProp _ = size
    setProp _ s lg = lg { size = s }

instance HasProp Tag (Group ms) where
    type Prop Tag (Group ms) = Bool
    getProp _ = tag
    setProp _ t lg = lg { tag = t }
