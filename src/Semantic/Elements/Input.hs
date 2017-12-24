{-# LANGUAGE ScopedTypeVariables #-}
module Semantic.Elements.Input where

import GHC.Generics as G
import Pure.View as View hiding (Button,Label,Input)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Button
import Semantic.Elements.Icon
import Semantic.Elements.Label

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
                    ( HostRef ((focused #) . _focus)
                    : Disabled disabled 
                    : Type _type 
                    : index 
                    : onInputChange onChange 
                    : fs
                    ) 
                    cs
            
            addInputProps c = c

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
                : attributes
                )
                ( mapPures addInputProps children )