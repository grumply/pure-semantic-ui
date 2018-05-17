{-# LANGUAGE NoMonomorphismRestriction #-}
module Semantic.Button
  ( module Properties
  , module Tools
  , Button(..), pattern Semantic.Button.Button
  , Content(..), pattern Content
  , Group(..), pattern Group
  , Or(..), pattern Or
  ) where

import GHC.Generics as G (Generic,to)
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML as HTML
import qualified Pure.Data.HTML.Properties as HTML

import Semantic.Utils

import Semantic.Icon hiding (Group(..), pattern Group)
import Semantic.Label hiding (Group(..), pattern Group)

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern Animated, Animated(..)
  , pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Basic, Basic(..)
  , pattern Circular, Circular(..)
  , pattern Color, Color(..)
  , pattern Compact, Compact(..)
  , pattern Disabled, Disabled(..)
  , pattern Floated, Floated(..)
  , pattern Fluid, Fluid(..)
  , pattern Focus, Focus(..)
  , pattern Inverted, Inverted(..)
  , pattern LabelPosition, LabelPosition(..)
  , pattern Loading, Loading(..)
  , pattern Negative, Negative(..)
  , pattern Positive, Positive(..)
  , pattern Primary, Primary(..)
  , pattern Secondary, Secondary(..)
  , pattern Size, Size(..)
  , pattern TabIndex, TabIndex(..)
  , pattern Toggle, Toggle(..)
  , pattern Hidden, Hidden(..)
  , pattern Visible, Visible(..)
  , pattern Labeled, Labeled(..)
  , pattern Vertical, Vertical(..)
  , pattern Widths, Widths(..)
  , pattern Localize, Localize(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

import Data.List as List (null,filter,partition,head)

data Button = Button_
  { as :: Features -> [View] -> View
  , children :: [View]
  , features :: Features
  , active :: Bool
  , animated :: Maybe Txt
  , attached :: Maybe Txt
  , basic :: Bool
  , circular :: Bool
  , color :: Txt
  , compact :: Bool
  , disabled :: Bool
  , floated :: Txt
  , fluid :: Bool
  , inverted :: Bool
  , labelPosition :: Txt
  , loading :: Bool
  , negative :: Bool
  , positive :: Bool
  , primary :: Bool
  , secondary :: Bool
  , size :: Txt
  , tabIndex :: Maybe Int
  , toggle :: Bool
  , focus :: Bool
  } deriving (Generic)

instance Default Button where
    def = (G.to gdef) { as = \fs cs -> HTML.Button & Features fs & Children cs }

pattern Button :: Button -> Button
pattern Button b = b

instance Pure Button where
    view Button_ {..} =
        let baseClasses xs =
              ( color
              : size
              : active # "active"
              : basic # "basic"
              : circular # "circular"
              : compact # "compact"
              : fluid # "fluid"
              : hasIcon # "icon"
              : inverted # "inverted"
              : loading # "loading"
              : negative # "negative"
              : positive # "positive"
              : primary # "primary"
              : secondary # "secondary"
              : toggle # "toggle"
              : (maybe def ("animated" <<>>) animated)
              : (maybe def ("attached" <<>>) attached)
              : xs
              )

            hasIcon = not $ List.null $ List.filter isIcon children
              where
                isIcon (View Icon_ {}) = True
                isIcon _ = False

            labels, children' :: [View]
            (labels,children') = List.partition isLabel children
              where
                isLabel (View Label_ {}) = True
                isLabel _ = False

            labeled = not $ Prelude.null labels

            labeledClasses xs
              | labelPosition /= def = (labelPosition <<>> "labeled") : xs
              | labeled              = "labeled" : xs
              | otherwise            = xs

            wrapperClasses xs =
                  disabled # "disabled"
                : (floated /= def) # (floated <>> "floated")
                : xs

            isButton =
                case as def def of
                    HTMLView _ "button" _ _ -> True
                    _                       -> False

            fs | labeled   = features & AddClasses ("ui" : labeledClasses ("button" : wrapperClasses []))
               | otherwise = features & AddClasses ("ui" : baseClasses (wrapperClasses (labeledClasses ["button"])))
                                      & ((disabled && isButton) ? HTML.Disabled "true" $ id)
                                      & HTML.Role "button"
                                      & (disabled ? HTML.TabIndex "-1"
                                                  $ maybe id (HTML.TabIndex . toTxt) tabIndex
                                        )

            cs | not labeled = children
               | otherwise   =
                  [ (labelPosition == "left") # List.head labels
                  , HTML.Button <| bfs |> children'
                  , (labelPosition == "right") # List.head labels
                  ]
                  where
                    bfs = AddClasses ("ui" : baseClasses ["button"])
                        . HTML.Disabled (disabled ? "true" $ "false")
                        . (disabled ? HTML.TabIndex "-1"
                                    $ maybe id (HTML.TabIndex . toTxt) tabIndex
                          )

        in
            as fs cs

instance HasProp Animated Button where
    type Prop Animated Button = Maybe Txt
    getProp _ b = animated b
    setProp _ anim b = b { animated = anim }

instance HasProp Active Button where
    type Prop Active Button = Bool
    getProp _ = active
    setProp _ a b = b { active = a }

instance HasProp As Button where
    type Prop As Button = Features -> [View] -> View
    getProp _ = as
    setProp _ f b = b { as = f }

instance HasProp Attached Button where
    type Prop Attached Button = Maybe Txt
    getProp _ = attached
    setProp _ attach b = b { attached = attach }

instance HasFeatures Button where
    getFeatures = features
    setFeatures cs b = b { features = cs }

instance HasProp Basic Button where
    type Prop Basic Button = Bool
    getProp _ = basic
    setProp _ bsc b = b { basic = bsc }

instance HasChildren Button where
    getChildren = children
    setChildren cs b = b { children = cs }

instance HasProp Circular Button where
    type Prop Circular Button = Bool
    getProp _ = circular
    setProp _ c b = b { circular = c }

instance HasProp Color Button where
    type Prop Color Button = Txt
    getProp _ = color
    setProp _ c b = b { color = c }

instance HasProp Compact Button where
    type Prop Compact Button = Bool
    getProp _ = compact
    setProp _ c b = b { compact = c }

instance HasProp Disabled Button where
    type Prop Disabled Button = Bool
    getProp _ = disabled
    setProp _ d b = b { disabled = d }

instance HasProp Floated Button where
    type Prop Floated Button = Txt
    getProp _ = floated
    setProp _ f b = b { floated = f }

instance HasProp Fluid Button where
    type Prop Fluid Button = Bool
    getProp _ = fluid
    setProp _ f b = b { fluid = f }

instance HasProp Focus Button where
    type Prop Focus Button = Bool
    getProp _ = focus
    setProp _ f b = b { focus = f }

instance HasProp Inverted Button where
    type Prop Inverted Button = Bool
    getProp _ = inverted
    setProp _ i b = b { inverted = i }

instance HasProp LabelPosition Button where
    type Prop LabelPosition Button = Txt
    getProp _ = labelPosition
    setProp _ lp b = b { labelPosition = lp }

instance HasProp Loading Button where
    type Prop Loading Button = Bool
    getProp _ = loading
    setProp _ l b = b { loading = l }

instance HasProp Negative Button where
    type Prop Negative Button = Bool
    getProp _ = negative
    setProp _ n b = b { negative = n }

instance HasProp Positive Button where
    type Prop Positive Button = Bool
    getProp _ = positive
    setProp _ p b = b { positive = p }

instance HasProp Primary Button where
    type Prop Primary Button = Bool
    getProp _ = primary
    setProp _ p b = b { primary = p }

instance HasProp Secondary Button where
    type Prop Secondary Button = Bool
    getProp _ = secondary
    setProp _ s b = b { secondary = s }

instance HasProp Size Button where
    type Prop Size Button = Txt
    getProp _ = size
    setProp _ s b = b { size = s }

instance HasProp TabIndex Button where
    type Prop TabIndex Button = Maybe Int
    getProp _ = tabIndex
    setProp _ ti b = b { tabIndex = ti }

instance HasProp Toggle Button where
    type Prop Toggle Button = Bool
    getProp _ = toggle
    setProp _ t b = b { toggle = t }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , hidden :: Bool
    , visible :: Bool
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> View
pattern Content bc = View bc

instance Pure Content where
    view Content_ {..} =
        let
            cs =
                [ hidden # "hidden"
                , visible # "visible"
                , "content"
                ]

        in
            as (features & AddClasses cs) children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ f bc = bc { as = f }

instance HasFeatures Content where
    getFeatures = features
    setFeatures fs bc = bc { features = fs }

instance HasChildren Content where
    getChildren = children
    setChildren cs bc = bc { children = cs }

instance HasProp Hidden Content where
    type Prop Hidden Content = Bool
    getProp _ = hidden
    setProp _ h bc = bc { hidden = h }

instance HasProp Visible Content where
    type Prop Visible Content = Bool
    getProp _ = visible
    setProp _ v bc = bc { visible = v }

data Group = Group_
    { as :: Features -> [View] -> View
    , attached :: Maybe Txt
    , features :: Features
    , basic :: Bool
    , children :: [View]
    , color :: Txt
    , compact :: Bool
    , floated :: Txt
    , fluid :: Bool
    , inverted :: Bool
    , labeled :: Bool
    , negative :: Bool
    , positive :: Bool
    , primary :: Bool
    , secondary :: Bool
    , size :: Txt
    , toggle :: Bool
    , vertical :: Bool
    , widths :: Txt
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> View
pattern Group bc = View bc

instance Pure Group where
    view Group_ {..} =
        let
            icon =
                foldPures (\(Icon_ {}) -> const True) False children

            cs =
                [ "ui"
                , color
                , size
                , basic # "basic"
                , compact # "compact"
                , fluid # "fluid"
                , icon # "icon"
                , inverted # "inverted"
                , labeled # "labeled"
                , negative # "negative"
                , positive # "positive"
                , primary # "primary"
                , secondary # "secondary"
                , toggle # "toggle"
                , vertical # "vertical"
                , useKeyOrValueAndKey attached "attached"
                , (floated /= def) # ("floated" <<>> floated)
                , widthProp widths def def
                , "buttons"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ f bg = bg { as = f }

instance HasProp Attached Group where
    type Prop Attached Group = Maybe Txt
    getProp _ = attached
    setProp _ attach bg = bg { attached = attach }

instance HasFeatures Group where
    getFeatures = features
    setFeatures fs bg = bg { features = fs }

instance HasProp Basic Group where
    type Prop Basic Group = Bool
    getProp _ = basic
    setProp _ b bg = bg { basic = b }

instance HasChildren Group where
    getChildren = children
    setChildren cs bg = bg { children = cs }

instance HasProp Color Group where
    type Prop Color Group = Txt
    getProp _ = color
    setProp _ c bg = bg { color = c }

instance HasProp Compact Group where
    type Prop Compact Group = Bool
    getProp _ = compact
    setProp _ c bg = bg { compact = c }

instance HasProp Floated Group where
    type Prop Floated Group = Txt
    getProp _ = floated
    setProp _ f bg = bg { floated = f }

instance HasProp Fluid Group where
    type Prop Fluid Group = Bool
    getProp _ = fluid
    setProp _ f bg = bg { fluid = f }

instance HasProp Inverted Group where
    type Prop Inverted Group = Bool
    getProp _ = inverted
    setProp _ i bg = bg { inverted = i }

instance HasProp Labeled Group where
    type Prop Labeled Group = Bool
    getProp _ = labeled
    setProp _ l bg = bg { labeled = l }

instance HasProp Negative Group where
    type Prop Negative Group = Bool
    getProp _ = negative
    setProp _ n bg = bg { negative = n }

instance HasProp Positive Group where
    type Prop Positive Group = Bool
    getProp _ = positive
    setProp _ p bg = bg { positive = p }

instance HasProp Primary Group where
    type Prop Primary Group = Bool
    getProp _ = primary
    setProp _ p bg = bg { primary = p }

instance HasProp Secondary Group where
    type Prop Secondary Group = Bool
    getProp _ = secondary
    setProp _ s bg = bg { secondary = s }

instance HasProp Size Group where
    type Prop Size Group = Txt
    getProp _ = size
    setProp _ s bg = bg { size = s }

instance HasProp Toggle Group where
    type Prop Toggle Group = Bool
    getProp _ = toggle
    setProp _ t bg = bg { toggle = t }

instance HasProp Vertical Group where
    type Prop Vertical Group = Bool
    getProp _ = vertical
    setProp _ v bg = bg { vertical = v }

instance HasProp Widths Group where
    type Prop Widths Group = Txt
    getProp _ = widths
    setProp _ w bg = bg { widths = w }

data Or = Or_
    { as :: Features -> [View] -> View
    , features :: Features
    , localize :: Txt
    } deriving (Generic)

instance Default Or where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Or :: Or -> View
pattern Or bo = View bo

instance Pure Or where
    view Or_ {..} = as (features & Class "or" & Property ("data-text",localize)) []

instance HasProp As Or where
    type Prop As Or = Features -> [View] -> View
    getProp _ = as
    setProp _ f bo = bo { as = f }

instance HasFeatures Or where
    getFeatures = features
    setFeatures fs bo = bo { features = fs }

instance HasProp Localize Or where
    type Prop Localize Or = Txt
    getProp _ = localize
    setProp _ l bo = bo { localize = l }
