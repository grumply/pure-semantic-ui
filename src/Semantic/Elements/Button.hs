module Semantic.Elements.Button (module Semantic.Elements.Button, module Export) where

import GHC.Generics as G
import Pure.View hiding (active,color,disabled,onClick,Button,Disabled,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon
import Semantic.Elements.Label

import Semantic.Elements.Button.ButtonContent as Export
import Semantic.Elements.Button.ButtonGroup as Export
import Semantic.Elements.Button.ButtonOr as Export

import Semantic.Properties.Active
import Semantic.Properties.Animated
import Semantic.Properties.As
import Semantic.Properties.Attached
import Semantic.Properties.Attributes
import Semantic.Properties.Basic
import Semantic.Properties.Children
import Semantic.Properties.Circular
import Semantic.Properties.Classes
import Semantic.Properties.Color
import Semantic.Properties.Compact
import Semantic.Properties.Disabled
import Semantic.Properties.Floated
import Semantic.Properties.Fluid
import Semantic.Properties.Focus
import Semantic.Properties.Inverted
import Semantic.Properties.LabelPosition
import Semantic.Properties.Loading
import Semantic.Properties.Negative
import Semantic.Properties.OnClick
import Semantic.Properties.Positive
import Semantic.Properties.Primary
import Semantic.Properties.Secondary
import Semantic.Properties.Size
import Semantic.Properties.TabIndex

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

pattern Button :: Typeable ms => Button ms -> View ms
pattern Button b = View b

instance Typeable ms => Pure Button ms where
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
                      ( ClassList cs
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