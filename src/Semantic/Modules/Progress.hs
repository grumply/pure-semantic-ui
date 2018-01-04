module Semantic.Modules.Progress where

import Data.Maybe
import GHC.Generics as G
import Pure.View

import Semantic.Utils

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

pattern Progress :: Typeable ms => Progress ms -> View ms
pattern Progress p = View p

instance Typeable ms => Pure Progress ms where
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
                ( ClassList cs
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