module Semantic.Elements.Button
  ( module Properties
  , module Tools
  , Button(..), pattern Button
  , Content(..), pattern Content
  , Group(..), pattern Group
  , Or(..), pattern Or
  ) where

import GHC.Generics as G
import Pure.View hiding (active,color,disabled,onClick,Button,Disabled,Label,Or,widths,hidden,vertical,visible,Content)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon hiding (Group(..), pattern Group)
import Semantic.Elements.Label hiding (Group(..), pattern Group)

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern Animated, Animated(..)
  , pattern As, As(..)
  , pattern Attached, Attached(..)
  , pattern Attributes, Attributes(..)
  , pattern Basic, Basic(..)
  , pattern Children, Children(..)
  , pattern Circular, Circular(..)
  , pattern Classes, Classes(..)
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
  , pattern OnClick, OnClick(..)
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
  )

data Button ms = Button_
  { as :: [Feature ms] -> [View ms] -> View ms
  , children :: [View ms]
  , attributes :: [Feature ms]
  , classes :: [Txt]
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
  , onClick :: Ef ms IO ()
  , positive :: Bool
  , primary :: Bool
  , secondary :: Bool
  , size :: Txt
  , tabIndex :: Maybe Int
  , toggle :: Bool
  , focus :: Bool
  } deriving (Generic)

instance Default (Button ms) where
    def = (G.to gdef) { as = HTML.Button }

pattern Button :: Button ms -> View ms
pattern Button b = View b

instance Pure Button ms where
    render Button_ {..} =
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
              : (may ("animated" <<>>) animated)
              : (may ("attached" <<>>) attached)
              : xs
              )

            hasIcon =
                foldPures (\(Icon_ {}) -> const True) False children

            (label,children') =
                foldr (\a (st,cs) ->
                    case a of
                        View Label {} -> (a,cs)
                        _             -> (st,a:cs)
                ) (nil,nil) children

            labeledClasses xs =
                ( (labelPosition
                    ? (labelPosition <<>> "labeled")
                    $ (label # "labeled")
                  )
                : xs
                )

            wrapperClasses xs =
                ( disabled # "disabled"
                : floated # "floated"
                : xs
                )

            index :: Feature ms
            index =
                disabled
                  ? Tabindex (-1)
                  $ may Tabindex tabIndex

            isButton =
                case as [] [] of
                    HTML.Button _ _ -> True
                    _ -> False

        in
            let cs = "ui" : baseClasses (wrapperClasses (labeledClasses ("button" : classes)))
                buttonClasses = "ui" : baseClasses ("button" : classes)
                containerClasses = "ui" : labeledClasses ("button" : classes ++ wrapperClasses [])
            in
                label
                  ? as
                      ( ClassList containerClasses
                      : onClick # (On "click" def { preventDef = True } (\_ -> return $ Just onClick))
                      : attributes
                      )
                      [ (labelPosition == "left") # label
                      , HTML.Button
                          [ ClassList buttonClasses
                          , HTML.Disabled disabled
                          , index
                          ]
                          children'
                      , (labelPosition == "right") # label
                      ]
                  $ as
                      ( mergeClasses $ ClassList cs
                      : (disabled && isButton) # HTML.Disabled True
                      : onClick # (On "click" def (\_ -> return $ Just onClick))
                      : Role "button"
                      : index
                      : attributes
                      )
                      children

instance HasProp Animated (Button ms) where
    type Prop Animated (Button ms) = Maybe Txt
    getProp _ b = animated b
    setProp _ anim b = b { animated = anim }

instance HasProp Active (Button ms) where
    type Prop Active (Button ms) = Bool
    getProp _ = active
    setProp _ a b = b { active = a }

instance HasProp As (Button ms) where
    type Prop As (Button ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f b = b { as = f }

instance HasProp Attached (Button ms) where
    type Prop Attached (Button ms) = Maybe Txt
    getProp _ = attached
    setProp _ attach b = b { attached = attach }

instance HasProp Attributes (Button ms) where
    type Prop Attributes (Button ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs b = b { attributes = cs }

instance HasProp Basic (Button ms) where
    type Prop Basic (Button ms) = Bool
    getProp _ = basic
    setProp _ bsc b = b { basic = bsc }

instance HasProp Children (Button ms) where
    type Prop Children (Button ms) = [View ms]
    getProp _ = children
    setProp _ cs b = b { children = cs }

instance HasProp Circular (Button ms) where
    type Prop Circular (Button ms) = Bool
    getProp _ = circular
    setProp _ c b = b { circular = c }

instance HasProp Classes (Button ms) where
    type Prop Classes (Button ms) = [Txt]
    getProp _ = classes
    setProp _ cs b = b { classes = cs }

instance HasProp Color (Button ms) where
    type Prop Color (Button ms) = Txt
    getProp _ = color
    setProp _ c b = b { color = c }

instance HasProp Compact (Button ms) where
    type Prop Compact (Button ms) = Bool
    getProp _ = compact
    setProp _ c b = b { compact = c }

instance HasProp Disabled (Button ms) where
    type Prop Disabled (Button ms) = Bool
    getProp _ = disabled
    setProp _ d b = b { disabled = d }

instance HasProp Floated (Button ms) where
    type Prop Floated (Button ms) = Txt
    getProp _ = floated
    setProp _ f b = b { floated = f }

instance HasProp Fluid (Button ms) where
    type Prop Fluid (Button ms) = Bool
    getProp _ = fluid
    setProp _ f b = b { fluid = f }

instance HasProp Focus (Button ms) where
    type Prop Focus (Button ms) = Bool
    getProp _ = focus
    setProp _ f b = b { focus = f }

instance HasProp OnClick (Button ms) where
    type Prop OnClick (Button ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ f b = b { onClick = f }

instance HasProp Inverted (Button ms) where
    type Prop Inverted (Button ms) = Bool
    getProp _ = inverted
    setProp _ i b = b { inverted = i }

instance HasProp LabelPosition (Button ms) where
    type Prop LabelPosition (Button ms) = Txt
    getProp _ = labelPosition
    setProp _ lp b = b { labelPosition = lp }

instance HasProp Loading (Button ms) where
    type Prop Loading (Button ms) = Bool
    getProp _ = loading
    setProp _ l b = b { loading = l }

instance HasProp Negative (Button ms) where
    type Prop Negative (Button ms) = Bool
    getProp _ = negative
    setProp _ n b = b { negative = n }

instance HasProp Positive (Button ms) where
    type Prop Positive (Button ms) = Bool
    getProp _ = positive
    setProp _ p b = b { positive = p }

instance HasProp Primary (Button ms) where
    type Prop Primary (Button ms) = Bool
    getProp _ = primary
    setProp _ p b = b { primary = p }

instance HasProp Secondary (Button ms) where
    type Prop Secondary (Button ms) = Bool
    getProp _ = secondary
    setProp _ s b = b { secondary = s }

instance HasProp Size (Button ms) where
    type Prop Size (Button ms) = Txt
    getProp _ = size
    setProp _ s b = b { size = s }

instance HasProp TabIndex (Button ms) where
    type Prop TabIndex (Button ms) = Maybe Int
    getProp _ = tabIndex
    setProp _ ti b = b { tabIndex = ti }

instance HasProp Toggle (Button ms) where
    type Prop Toggle (Button ms) = Bool
    getProp _ = toggle
    setProp _ t b = b { toggle = t }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , hidden :: Bool
    , visible :: Bool
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content bc = View bc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( hidden # "hidden"
                : visible # "visible"
                : "content"
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Content ms) where
    type Prop As (Content ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f bc = bc { as = f }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs bc = bc { attributes = cs }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs bc = bc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs bc = bc { classes = cs }

instance HasProp Hidden (Content ms) where
    type Prop Hidden (Content ms) = Bool
    getProp _ = hidden
    setProp _ h bc = bc { hidden = h }

instance HasProp Visible (Content ms) where
    type Prop Visible (Content ms) = Bool
    getProp _ = visible
    setProp _ v bc = bc { visible = v }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attached :: Maybe Txt
    , attributes :: [Feature ms]
    , basic :: Bool
    , children :: [View ms]
    , classes :: [Txt]
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

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group bc = View bc

instance Pure Group ms where
    render Group_ {..} =
        let
            icon =
                foldPures (\(Icon_ {}) -> const True) False children

            cs =
                ( "ui"
                : color
                : size
                : basic # "basic"
                : compact # "compact"
                : fluid # "fluid"
                : icon # "icon"
                : inverted # "inverted"
                : labeled # "labeled"
                : negative # "negative"
                : positive # "positive"
                : primary # "primary"
                : secondary # "secondary"
                : toggle # "toggle"
                : vertical # "vertical"
                : useKeyOrValueAndKey attached "attached"
                : floated # ("floated" <<>> floated)
                : widthProp widths def def
                : "buttons"
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
    setProp _ f bg = bg { as = f }

instance HasProp Attached (Group ms) where
    type Prop Attached (Group ms) = Maybe Txt
    getProp _ = attached
    setProp _ attach bg = bg { attached = attach }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs bg = bg { attributes = cs }

instance HasProp Basic (Group ms) where
    type Prop Basic (Group ms) = Bool
    getProp _ = basic
    setProp _ b bg = bg { basic = b }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs bg = bg { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs bg = bg { classes = cs }

instance HasProp Color (Group ms) where
    type Prop Color (Group ms) = Txt
    getProp _ = color
    setProp _ c bg = bg { color = c }

instance HasProp Compact (Group ms) where
    type Prop Compact (Group ms) = Bool
    getProp _ = compact
    setProp _ c bg = bg { compact = c }

instance HasProp Floated (Group ms) where
    type Prop Floated (Group ms) = Txt
    getProp _ = floated
    setProp _ f bg = bg { floated = f }

instance HasProp Fluid (Group ms) where
    type Prop Fluid (Group ms) = Bool
    getProp _ = fluid
    setProp _ f bg = bg { fluid = f }

instance HasProp Inverted (Group ms) where
    type Prop Inverted (Group ms) = Bool
    getProp _ = inverted
    setProp _ i bg = bg { inverted = i }

instance HasProp Labeled (Group ms) where
    type Prop Labeled (Group ms) = Bool
    getProp _ = labeled
    setProp _ l bg = bg { labeled = l }

instance HasProp Negative (Group ms) where
    type Prop Negative (Group ms) = Bool
    getProp _ = negative
    setProp _ n bg = bg { negative = n }

instance HasProp Positive (Group ms) where
    type Prop Positive (Group ms) = Bool
    getProp _ = positive
    setProp _ p bg = bg { positive = p }

instance HasProp Primary (Group ms) where
    type Prop Primary (Group ms) = Bool
    getProp _ = primary
    setProp _ p bg = bg { primary = p }

instance HasProp Secondary (Group ms) where
    type Prop Secondary (Group ms) = Bool
    getProp _ = secondary
    setProp _ s bg = bg { secondary = s }

instance HasProp Size (Group ms) where
    type Prop Size (Group ms) = Txt
    getProp _ = size
    setProp _ s bg = bg { size = s }

instance HasProp Toggle (Group ms) where
    type Prop Toggle (Group ms) = Bool
    getProp _ = toggle
    setProp _ t bg = bg { toggle = t }

instance HasProp Vertical (Group ms) where
    type Prop Vertical (Group ms) = Bool
    getProp _ = vertical
    setProp _ v bg = bg { vertical = v }

instance HasProp Widths (Group ms) where
    type Prop Widths (Group ms) = Txt
    getProp _ = widths
    setProp _ w bg = bg { widths = w }

data Or ms = Or_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , localize :: Txt
    } deriving (Generic)

instance Default (Or ms) where
    def = (G.to gdef) { as = Div }

pattern Or :: Or ms -> View ms
pattern Or bo = View bo

instance Pure Or ms where
    render Or_ {..} =
        as
            ( ClassList ( "or" : classes )
            : localize # Attr "data-text" localize
            : attributes
            )
            []

instance HasProp As (Or ms) where
    type Prop As (Or ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f bo = bo { as = f }

instance HasProp Attributes (Or ms) where
    type Prop Attributes (Or ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs bo = bo { attributes = cs }

instance HasProp Classes (Or ms) where
    type Prop Classes (Or ms) = [Txt]
    getProp _ = classes
    setProp _ cs bo = bo { classes = cs }

instance HasProp Localize (Or ms) where
    type Prop Localize (Or ms) = Txt
    getProp _ = localize
    setProp _ l bo = bo { localize = l }
