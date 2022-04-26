module Semantic.Progress
  ( module Properties
  , module Tools
  , Progress(..), pattern Semantic.Progress.Progress
  ) where

import Pure hiding (color,size,min,max,percent,disabled,active,(#))
import qualified Pure

import Data.Maybe
import Data.Monoid
import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
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
  , pattern ShowProgress, ShowProgress(..)
  , pattern Size, Size(..)
  , pattern Success, Success(..)
  , pattern Total, Total(..)
  , pattern Value, Value(..)
  , pattern Warning, Warning(..)
  )

import Prelude hiding (error)

import Data.Function as Tools ((&))

data Progress = Progress_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
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
    , progress :: Bool
    , size :: Txt
    , success :: Bool
    , total :: Int
    , value :: Int
    , warning :: Bool
    } deriving (Generic)

instance Default Progress where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Progress :: Progress -> Progress
pattern Progress p = p

instance Pure Progress where
    view Progress_ {..} =
        let
            decimals p x = (fromInteger $ Prelude.round $ x * (10^p)) / (10.0 Prelude.^^ p)

            totalPercent =
                (total > 0)
                    ? (fromIntegral value / fromIntegral total * 100)
                    $ 100

            isAutoSuccess = autoSuccess && ((isJust percent) ? (percent >= Just 100) $ (value >= total))

            getPercent = ((precision >= 0) ? decimals precision $ id)
                (max 0 (min 100 (fromMaybe totalPercent percent)))

            cs =
                [ "ui"
                , color
                , size
                , (active || indicating) # "active"
                , disabled # "disabled"
                , error # "error"
                , indicating # "indicating"
                , inverted # "inverted"
                , (success || isAutoSuccess) # "success"
                , warning # "warning"
                , (attached /= mempty) # (attached <>> "attached")
                , "progress"
                ]
        in
            as (features & Classes cs)
                [ Div <| Class "bar" . Style width (Pure.percent getPercent) |>
                      [ progress #
                            Div <| Class "progress" |>
                                [ txt $ maybe (int value <> "/" <> int total) Pure.percent percent ]
                      ]
                , Div <| Class "label" |> children
                ]

instance HasProp As Progress where
    type Prop As Progress = Features -> [View] -> View
    getProp _ = as
    setProp _ f p = p { as = f }

instance HasFeatures Progress where
    getFeatures = features
    setFeatures cs p = p { features = cs }

instance HasChildren Progress where
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasProp Active Progress where
    type Prop Active Progress = Bool
    getProp _ = active
    setProp _ a p = p { active = a }

instance HasProp Attached Progress where
    type Prop Attached Progress = Txt
    getProp _ = attached
    setProp _ a p = p { attached = a }

instance HasProp AutoSuccess Progress where
    type Prop AutoSuccess Progress = Bool
    getProp _ = autoSuccess
    setProp _ as p = p { autoSuccess = as }

instance HasProp Color Progress where
    type Prop Color Progress = Txt
    getProp _ = color
    setProp _ c p = p { color = c }

instance HasProp Disabled Progress where
    type Prop Disabled Progress = Bool
    getProp _ = disabled
    setProp _ d p = p { disabled = d }

instance HasProp Error Progress where
    type Prop Error Progress = Bool
    getProp _ = error
    setProp _ e p = p { error = e }

instance HasProp Indicating Progress where
    type Prop Indicating Progress = Bool
    getProp _ = indicating
    setProp _ i p = p { indicating = i }

instance HasProp Inverted Progress where
    type Prop Inverted Progress = Bool
    getProp _ = inverted
    setProp _ i p = p { inverted = i }

instance HasProp Percent Progress where
    type Prop Percent Progress = Maybe Double
    getProp _ = percent
    setProp _ per p = p { percent = per }

instance HasProp Precision Progress where
    type Prop Precision Progress = Int
    getProp _ = precision
    setProp _ pre p = p { precision = pre }

instance HasProp ShowProgress Progress where
    type Prop ShowProgress Progress = Bool
    getProp _ = progress
    setProp _ sp p = p { progress = sp }

instance HasProp Size Progress where
    type Prop Size Progress = Txt
    getProp _ = size
    setProp _ s p = p { size = s }

instance HasProp Success Progress where
    type Prop Success Progress = Bool
    getProp _ = success
    setProp _ s p = p { success = s }

instance HasProp Total Progress where
    type Prop Total Progress = Int
    getProp _ = total
    setProp _ t p = p { total = t}

instance HasProp Value Progress where
    type Prop Value Progress = Int
    getProp _ = value
    setProp _ v p = p { value = v }

instance HasProp Warning Progress where
    type Prop Warning Progress = Bool
    getProp _ = warning
    setProp _ w p = p { warning = w }
