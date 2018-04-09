module Semantic.Modules.Progress
  ( module Properties
  , module Tools
  , Progress(..), pattern Progress
  ) where

import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (active,color,disabled,Progress,Value)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Active, Active(..)
  , pattern Attached, Attached(..)
  , pattern AutoSuccess, AutoSuccess(..)
  , pattern Color, Color(..)
  , pattern Disabled, Disabled(..)
  , pattern Error, Error(..)
  , pattern Indicating, Indicating(..)
  , pattern Inverted, Inverted(..)
  , pattern Percent, Percent(..)
  , pattern Precision, Precision(..)
  , pattern Size, Size(..)
  , pattern Success, Success(..)
  , pattern Total, Total(..)
  , pattern Value, Value(..)
  , pattern Warning, Warning(..)
  )

import Prelude hiding (error)

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Progress ms = Progress_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , attached :: Txt
    , autoSuccess :: Bool
    , color :: Txt
    , disabled :: Bool
    , error :: Bool
    , indicating :: Bool
    , inverted :: Bool
    , percent :: Maybe Double
    , precision :: Int
    , size :: Txt
    , success :: Bool
    , total :: Int
    , value :: Int
    , warning :: Bool
    } deriving (Generic)

instance Default (Progress ms) where
    def = (G.to gdef) { as = Div }

pattern Progress :: Progress ms -> View ms
pattern Progress p = View p

instance Pure Progress ms where
    render Progress_ {..} =
        let
            decimals p x = (fromInteger $ Prelude.round $ x * (10^p)) / (10.0^^p)

            totalPercent =
                total
                    ? (fromIntegral value / fromIntegral total * 100)
                    $ 100

            isAutoSuccess = autoSuccess && (percent ? (percent >= Just 100) $ (value >= total))

            getPercent = (precision ? decimals precision $ id)
                (max 0 (min 100 (fromMaybe totalPercent percent)))

            cs =
                ( "ui"
                : color
                : size
                : (active || indicating) # "active"
                : disabled # "disabled"
                : error # "error"
                : indicating # "indicating"
                : inverted # "inverted"
                : (success || isAutoSuccess) # "success"
                : warning # "warning"
                : attached # "attached"
                : "progress"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                [ Div [ ClassList [ "bar" ]
                      , StyleList [(width,per getPercent)]
                      ]
                      [ Div [ ClassList [ "progress" ] ]
                            [ fromTxt $ maybe (int value <> "/" <> int total) per percent ]
                      ]
                , Div [ ClassList [ "label" ] ] children
                ]


instance HasProp As (Progress ms) where
    type Prop As (Progress ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f p = p { as = f }

instance HasProp Attributes (Progress ms) where
    type Prop Attributes (Progress ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs p = p { attributes = cs }

instance HasProp Children (Progress ms) where
    type Prop Children (Progress ms) = [View ms]
    getProp _ = children
    setProp _ cs p = p { children = cs }

instance HasProp Classes (Progress ms) where
    type Prop Classes (Progress ms) = [Txt]
    getProp _ = classes
    setProp _ cs p = p { classes = cs }

instance HasProp Active (Progress ms) where
    type Prop Active (Progress ms) = Bool
    getProp _ = active
    setProp _ a p = p { active = a }

instance HasProp Attached (Progress ms) where
    type Prop Attached (Progress ms) = Txt
    getProp _ = attached
    setProp _ a p = p { attached = a }

instance HasProp AutoSuccess (Progress ms) where
    type Prop AutoSuccess (Progress ms) = Bool
    getProp _ = autoSuccess
    setProp _ as p = p { autoSuccess = as }

instance HasProp Color (Progress ms) where
    type Prop Color (Progress ms) = Txt
    getProp _ = color
    setProp _ c p = p { color = c }

instance HasProp Disabled (Progress ms) where
    type Prop Disabled (Progress ms) = Bool
    getProp _ = disabled
    setProp _ d p = p { disabled = d }

instance HasProp Error (Progress ms) where
    type Prop Error (Progress ms) = Bool
    getProp _ = error
    setProp _ e p = p { error = e }

instance HasProp Indicating (Progress ms) where
    type Prop Indicating (Progress ms) = Bool
    getProp _ = indicating
    setProp _ i p = p { indicating = i }

instance HasProp Inverted (Progress ms) where
    type Prop Inverted (Progress ms) = Bool
    getProp _ = inverted
    setProp _ i p = p { inverted = i }

instance HasProp Percent (Progress ms) where
    type Prop Percent (Progress ms) = Maybe Double
    getProp _ = percent
    setProp _ per p = p { percent = per }

instance HasProp Precision (Progress ms) where
    type Prop Precision (Progress ms) = Int
    getProp _ = precision
    setProp _ pre p = p { precision = pre }

instance HasProp Size (Progress ms) where
    type Prop Size (Progress ms) = Txt
    getProp _ = size
    setProp _ s p = p { size = s }

instance HasProp Success (Progress ms) where
    type Prop Success (Progress ms) = Bool
    getProp _ = success
    setProp _ s p = p { success = s }

instance HasProp Total (Progress ms) where
    type Prop Total (Progress ms) = Int
    getProp _ = total
    setProp _ t p = p { total = t}

instance HasProp Value (Progress ms) where
    type Prop Value (Progress ms) = Int
    getProp _ = value
    setProp _ v p = p { value = v }

instance HasProp Warning (Progress ms) where
    type Prop Warning (Progress ms) = Bool
    getProp _ = warning
    setProp _ w p = p { warning = w }
