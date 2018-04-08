module Semantic.Modules.Progress where

import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (active,color,disabled)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActiveProp(..), pattern Active
  , HasAttachedProp(..), pattern Attached
  , HasAutoSuccessProp(..), pattern AutoSuccess
  , HasColorProp(..), pattern Color
  , HasDisabledProp(..), pattern Disabled
  , HasErrorProp(..), pattern Error
  , HasIndicatingProp(..), pattern Indicating
  , HasInvertedProp(..), pattern Inverted
  , HasPercentProp(..), pattern Percent
  , HasPrecisionProp(..), pattern Precision
  , HasSizeProp(..), pattern Size
  , HasSuccessProp(..), pattern Success
  , HasTotalProp(..), pattern Total
  , HasValueProp(..), pattern Value
  , HasWarningProp(..), pattern Warning
  )

import Prelude hiding (error)

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


instance HasAsProp (Progress ms) where
    type AsProp (Progress ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f p = p { as = f }

instance HasAttributesProp (Progress ms) where
    type Attribute (Progress ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs p = p { attributes = cs }

instance HasChildrenProp (Progress ms) where
    type Child (Progress ms) = View ms
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasClassesProp (Progress ms) where
    getClasses = classes
    setClasses cs p = p { classes = cs }

instance HasActiveProp (Progress ms) where
    getActive = active
    setActive a p = p { active = a }

instance HasAttachedProp (Progress ms) where
    type AttachedProp (Progress ms) = Txt
    getAttached = attached
    setAttached a p = p { attached = a }

instance HasAutoSuccessProp (Progress ms) where
    getAutoSuccess = autoSuccess
    setAutoSuccess as p = p { autoSuccess = as }

instance HasColorProp (Progress ms) where
    getColor = color
    setColor c p = p { color = c }

instance HasDisabledProp (Progress ms) where
    getDisabled = disabled
    setDisabled d p = p { disabled = d }

instance HasErrorProp (Progress ms) where
    getError = error
    setError e p = p { error = e }

instance HasIndicatingProp (Progress ms) where
    getIndicating = indicating
    setIndicating i p = p { indicating = i }

instance HasInvertedProp (Progress ms) where
    getInverted = inverted
    setInverted i p = p { inverted = i }

instance HasPercentProp (Progress ms) where
    getPercent = percent
    setPercent per p = p { percent = per }

instance HasPrecisionProp (Progress ms) where
    getPrecision = precision
    setPrecision pre p = p { precision = pre }

instance HasSizeProp (Progress ms) where
    getSize = size
    setSize s p = p { size = s }

instance HasSuccessProp (Progress ms) where
    getSuccess = success
    setSuccess s p = p { success = s }

instance HasTotalProp (Progress ms) where
    getTotal = total
    setTotal t p = p { total = t}

instance HasValueProp (Progress ms) where
    type ValueProp (Progress ms) = Int
    getValue = value
    setValue v p = p { value = v }

instance HasWarningProp (Progress ms) where
    getWarning = warning
    setWarning w p = p { warning = w } 
