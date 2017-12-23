module Semantic.Elements.Button (module Semantic.Elements.Button, module Export) where

import GHC.Generics as G
import Pure.View hiding (Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon
import Semantic.Elements.Label

import Semantic.Elements.Button.ButtonContent as Export

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
                      : handleClick # (On "click" def { preventDef = True } (\_ -> return $ Just handleClick))
                      : attributes
                      )
                      [ (labelPosition == "left") # label
                      , HTML.Button 
                          [ ClassList buttonClasses
                          , Disabled disabled
                          , index
                          ]
                          children'
                      , (labelPosition == "right") # label
                      ]
                  $ as
                      ( ClassList cs
                      : (disabled && isButton) # Disabled True
                      : onClick handleClick
                      : Role "button"
                      : index
                      : attributes
                      )
                      children 


