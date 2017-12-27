{-# LANGUAGE ScopedTypeVariables #-}
module Semantic.Elements.Input where

import GHC.Generics as G
import Pure.View as View hiding (disabled,focused,transparent,Disabled,Button,Label,Input,Type)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Button
import Semantic.Elements.Icon
import Semantic.Elements.Label

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Disabled
import Semantic.Properties.Error
import Semantic.Properties.Fluid
import Semantic.Properties.Focus
import Semantic.Properties.Focused
import Semantic.Properties.Inverted
import Semantic.Properties.Loading
import Semantic.Properties.OnChange
import Semantic.Properties.Size
import Semantic.Properties.TabIndex
import Semantic.Properties.Transparent
import Semantic.Properties.Type

import Prelude hiding (error)

data Input ms = Input_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , disabled :: Bool
    , error :: Bool
    , fluid :: Bool
    , focus :: Bool
    , focused :: Bool
    , inverted :: Bool
    , loading :: Bool
    , onChange :: Txt -> Ef ms IO ()
    , size :: Txt
    , tabIndex :: Maybe Int
    , transparent :: Bool
    , _type :: Txt
    } deriving (Generic)

instance Default (Input ms) where
    def = (G.to gdef) { as = Div, _type = "text" }

pattern Input :: Typeable ms => Input ms -> View ms
pattern Input i = View i

data InputFormatter = IF
  { inputSeen :: Bool
  , labelPosition :: Maybe Txt
  , iconPosition :: Maybe Txt
  , actionPosition :: Maybe Txt
  } deriving (Generic,Default)

calculatePositions :: forall ms. Typeable ms => [View ms] -> InputFormatter
calculatePositions = foldr analyze def
    where
        analyze :: View ms -> InputFormatter -> InputFormatter
        analyze (HTML.Input _ _) state = state { inputSeen = True }
        analyze (View Label{}) state
            | inputSeen state          = state { labelPosition = Just "" }
            | otherwise                = state { labelPosition = Just "left" }
        analyze (View Icon{}) state
            | inputSeen state          = state { iconPosition = Just "" }
            | otherwise                = state { iconPosition = Just "left" }
        analyze (View Button{}) state
            | inputSeen state          = state { actionPosition = Just "" }
            | otherwise                = state { actionPosition = Just "left" }
        analyze _ state                = state

instance Typeable ms => Pure Input ms where
    render Input_ {..} =
        let
            _focus e = do
                focusNode e
                return Nothing

            addInputProps :: View ms -> View ms
            addInputProps (HTML.Input fs cs) =
                HTML.Input 
                    (( HostRef ((focused #) . _focus)
                    : HTML.Disabled disabled 
                    : HTML.Type _type 
                    : index 
                    : onInput onChange 
                    : inputAttrs
                    ) ++ fs)
                    cs
            
            addInputProps c = c

            (inputAttrs,otherAttrs) = extractInputAttrs attributes

            index = maybe (disabled # Tabindex (-1)) Tabindex tabIndex

            IF {..} = calculatePositions children

            cs =
                ( "ui"
                : size
                : disabled # "disabled"
                : error # "error"
                : fluid # "fluid"
                : focus # "focus"
                : inverted # "inverted"
                : loading # "loading"
                : transparent # "transparent"
                : may (<>> "action")  actionPosition
                : may (<>> "icon")    iconPosition
                : may (<>> "labeled") labelPosition
                : "input"
                : classes
                )
        in
            as
                ( ClassList cs
                : otherAttrs
                )
                ( map addInputProps children )

instance HasAsProp (Input ms) where
    type AsProp (Input ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f i = i { as = f }

instance HasAttributesProp (Input ms) where
    type Attribute (Input ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs i = i { attributes = cs }

instance HasOnChangeProp (Input ms) where
    type OnChangeProp (Input ms) = Txt -> Ef ms IO ()
    getOnChange = onChange
    setOnChange oc i = i { onChange = oc }

instance HasChildrenProp (Input ms) where
    type Child (Input ms) = View ms
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasClassesProp (Input ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }

instance HasDisabledProp (Input ms) where
    getDisabled = disabled
    setDisabled d i = i { disabled = d }

instance HasErrorProp (Input ms) where
    getError = error
    setError e i = i { error = e }

instance HasFluidProp (Input ms) where
    getFluid = fluid
    setFluid f i = i { fluid = f }

instance HasFocusProp (Input ms) where
    getFocus = focus
    setFocus f i = i { focus = f }

instance HasFocusedProp (Input ms) where
    getFocused = focused
    setFocused f i = i { focused = f }

instance HasInvertedProp (Input ms) where
    getInverted = inverted
    setInverted inv i = i { inverted = inv }

instance HasLoadingProp (Input ms) where
    getLoading = loading
    setLoading l i = i { loading = l }

instance HasSizeProp (Input ms) where
    getSize = size
    setSize s i = i { size = s }

instance HasTabIndexProp (Input ms) where
    getTabIndex = tabIndex
    setTabIndex ti i = i { tabIndex = ti }

instance HasTransparentProp (Input ms) where
    getTransparent = transparent
    setTransparent t i = i { transparent = t }

instance HasTypeProp (Input ms) where
    getType = _type
    setType t i = i { _type = t }