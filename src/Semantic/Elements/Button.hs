module Semantic.Elements.Button where

import GHC.Generics as G
import Pure.View hiding (active,color,disabled,onClick,Button,Disabled,Label,Or,widths,hidden,vertical,visible)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon hiding (Group(..))
import Semantic.Elements.Label hiding (Group(..))

import Semantic.Properties as Properties
  ( HasActiveProp(..), pattern Active
  , HasAnimatedProp(..), pattern Animated
  , HasAsProp(..), pattern As
  , HasAttachedProp(..), pattern Attached
  , HasAttributesProp(..), pattern Attributes
  , HasBasicProp(..), pattern Basic
  , HasChildrenProp(..), pattern Children
  , HasCircularProp(..), pattern Circular
  , HasClassesProp(..), pattern Classes
  , HasColorProp(..), pattern Color
  , HasCompactProp(..), pattern Compact
  , HasDisabledProp(..), pattern Disabled
  , HasFloatedProp(..), pattern Floated
  , HasFluidProp(..), pattern Fluid
  , HasFocusProp(..), pattern Focus
  , HasInvertedProp(..), pattern Inverted
  , HasLabelPositionProp(..), pattern LabelPosition
  , HasLoadingProp(..), pattern Loading
  , HasNegativeProp(..), pattern Negative
  , HasOnClickProp(..), pattern OnClick
  , HasPositiveProp(..), pattern Positive
  , HasPrimaryProp(..), pattern Primary
  , HasSecondaryProp(..), pattern Secondary
  , HasSizeProp(..), pattern Size
  , HasTabIndexProp(..), pattern TabIndex
  , HasToggleProp(..), pattern Toggle
  , HasHiddenProp(..), pattern Hidden
  , HasVisibleProp(..), pattern Visible
  , HasLabeledProp(..), pattern Labeled
  , HasVerticalProp(..), pattern Vertical
  , HasWidthsProp(..), pattern Widths
  , HasLocalizeProp(..), pattern Localize
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

instance HasAnimatedProp (Button ms) where
    type AnimatedProp (Button ms) = Maybe Txt
    getAnimated b = animated b
    setAnimated anim b = b { animated = anim }

instance HasActiveProp (Button ms) where
    getActive = active
    setActive a b = b { active = a }

instance HasAsProp (Button ms) where
    type AsProp (Button ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f b = b { as = f }

instance HasAttachedProp (Button ms) where
    type AttachedProp (Button ms) = Maybe Txt
    getAttached = attached
    setAttached attach b = b { attached = attach }

instance HasAttributesProp (Button ms) where
    type Attribute (Button ms) = Feature ms
    getAttributes = attributes
    setAttributes cs b = b { attributes = cs }

instance HasBasicProp (Button ms) where
    getBasic = basic
    setBasic bsc b = b { basic = bsc }

instance HasChildrenProp (Button ms) where
    type Child (Button ms) = View ms
    getChildren = children
    setChildren cs b = b { children = cs }

instance HasCircularProp (Button ms) where
    getCircular = circular
    setCircular c b = b { circular = c }

instance HasClassesProp (Button ms) where
    getClasses = classes
    setClasses cs b = b { classes = cs }

instance HasColorProp (Button ms) where
    getColor = color
    setColor c b = b { color = c }

instance HasCompactProp (Button ms) where
    getCompact = compact
    setCompact c b = b { compact = c }

instance HasDisabledProp (Button ms) where
    getDisabled = disabled
    setDisabled d b = b { disabled = d }

instance HasFloatedProp (Button ms) where
    getFloated = floated
    setFloated f b = b { floated = f }

instance HasFluidProp (Button ms) where
    getFluid = fluid
    setFluid f b = b { fluid = f }

instance HasFocusProp (Button ms) where
    getFocus = focus
    setFocus f b = b { focus = f }

instance HasOnClickProp (Button ms) where
    type OnClickProp (Button ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick f b = b { onClick = f }

instance HasInvertedProp (Button ms) where
    getInverted = inverted
    setInverted i b = b { inverted = i }

instance HasLabelPositionProp (Button ms) where
    getLabelPosition = labelPosition
    setLabelPosition lp b = b { labelPosition = lp }

instance HasLoadingProp (Button ms) where
    getLoading = loading
    setLoading l b = b { loading = l }

instance HasNegativeProp (Button ms) where
    getNegative = negative
    setNegative n b = b { negative = n }

instance HasPositiveProp (Button ms) where
    getPositive = positive
    setPositive p b = b { positive = p }

instance HasPrimaryProp (Button ms) where
    getPrimary = primary
    setPrimary p b = b { primary = p }

instance HasSecondaryProp (Button ms) where
    getSecondary = secondary
    setSecondary s b = b { secondary = s }

instance HasSizeProp (Button ms) where
    getSize = size
    setSize s b = b { size = s }

instance HasTabIndexProp (Button ms) where
    getTabIndex = tabIndex
    setTabIndex ti b = b { tabIndex = ti }

instance HasToggleProp (Button ms) where
    getToggle = toggle
    setToggle t b = b { toggle = t }

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

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f bc = bc { as = f }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes cs bc = bc { attributes = cs }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs bc = bc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs bc = bc { classes = cs }

instance HasHiddenProp (Content ms) where
    getHidden = hidden
    setHidden h bc = bc { hidden = h }

instance HasVisibleProp (Content ms) where
    getVisible = visible
    setVisible v bc = bc { visible = v }

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
    , widths :: Width
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

instance HasAsProp (Group ms) where
    type AsProp (Group ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f bg = bg { as = f }

instance HasAttachedProp (Group ms) where
    type AttachedProp (Group ms) = Maybe Txt
    getAttached = attached
    setAttached attach bg = bg { attached = attach }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes cs bg = bg { attributes = cs }

instance HasBasicProp (Group ms) where
    getBasic = basic
    setBasic b bg = bg { basic = b }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs bg = bg { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs bg = bg { classes = cs }

instance HasColorProp (Group ms) where
    getColor = color
    setColor c bg = bg { color = c }

instance HasCompactProp (Group ms) where
    getCompact = compact
    setCompact c bg = bg { compact = c }

instance HasFloatedProp (Group ms) where
    getFloated = floated
    setFloated f bg = bg { floated = f }

instance HasFluidProp (Group ms) where
    getFluid = fluid
    setFluid f bg = bg { fluid = f }

instance HasInvertedProp (Group ms) where
    getInverted = inverted
    setInverted i bg = bg { inverted = i }

instance HasLabeledProp (Group ms) where
    getLabeled = labeled
    setLabeled l bg = bg { labeled = l }

instance HasNegativeProp (Group ms) where
    getNegative = negative
    setNegative n bg = bg { negative = n }

instance HasPositiveProp (Group ms) where
    getPositive = positive
    setPositive p bg = bg { positive = p }

instance HasPrimaryProp (Group ms) where
    getPrimary = primary
    setPrimary p bg = bg { primary = p }

instance HasSecondaryProp (Group ms) where
    getSecondary = secondary
    setSecondary s bg = bg { secondary = s }

instance HasSizeProp (Group ms) where
    getSize = size
    setSize s bg = bg { size = s }

instance HasToggleProp (Group ms) where
    getToggle = toggle
    setToggle t bg = bg { toggle = t }

instance HasVerticalProp (Group ms) where
    getVertical = vertical
    setVertical v bg = bg { vertical = v }

instance HasWidthsProp (Group ms) where
    getWidths = widths
    setWidths w bg = bg { widths = w }

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

instance HasAsProp (Or ms) where
    type AsProp (Or ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f bo = bo { as = f }

instance HasAttributesProp (Or ms) where
    type Attribute (Or ms) = Feature ms
    getAttributes = attributes
    setAttributes cs bo = bo { attributes = cs }

instance HasClassesProp (Or ms) where
    getClasses = classes
    setClasses cs bo = bo { classes = cs }

instance HasLocalizeProp (Or ms) where
    getLocalize = localize
    setLocalize l bo = bo { localize = l }
