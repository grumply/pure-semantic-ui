module Semantic.Elements.Button where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

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
  , icon :: View ms
  , inverted :: Bool
  , label :: View ms
  , labelPosition :: Txt
  , loading :: Bool
  , negative :: Bool
  , handleClick :: Ef ms IO ()
  , positive :: Bool
  , primary :: Bool
  , secondary :: Bool 
  , size :: Txt
  , tabIndex :: Maybe Int
  , toggle :: Bool
  , focus :: Bool
  } deriving (Generic)

instance Default (Button ms) where
    def = (G.to gdef) { as = Div }

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
              : icon # "icon"
              : inverted # "inverted"
              : loading # "loading"
              : negative # "negative"
              : positive # "positive"
              : primary # "primary"
              : secondary # "secondary"
              : toggle # "toggle"
              : useKeyOrValueAndKey animated "animated" 
              : useKeyOrValueAndKey attached "attached"
              : xs
              )

            labeledClasses xs =
                ( labelPosition 
                  ? useKeyOrValueAndKey labelPosition "labeled"
                  $ (label # "labeled")
                : xs
                )

            wrapprClasses =
                [ disabled # "disabled"
                , floated # "floated"
                ] 

            index :: Feature ms
            index = 
                disabled
                  ? Tabindex (-1)
                  $ may Tabindex tabIndex 

            hasIconClass =
                let i = notNil icon
                    lp = notNil labelPosition
                    nc = isNil children
                in i && (lp || nc)

        in 
            if notNil label
                then
                    let buttonClasses = "ui" : baseClasses ("button" : classes)
                        containerClasses = "ui" : labeledClasses ("button" : classes ++ wrapperClasses)
                        -- labelElement = def { basic = True, pointing = labelPosition == "left" ? "right" $ "left" }
                    in
                        as 
                            ( ClassList 
                            : index 
                            : handleClick # (On "click" def { preventDef = True } (\_ -> return $ Just handleClick))
                            : attributes
                            )
                            ( 
                            : children
                            )
                                
