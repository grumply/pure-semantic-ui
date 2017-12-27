module Semantic.Elements.Button (module Semantic.Elements.Button, module Export) where

import GHC.Generics as G
import Pure.View hiding (active,Button,Label)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Icon
import Semantic.Elements.Label

import Semantic.Elements.Button.ButtonContent as Export
import Semantic.Elements.Button.ButtonGroup as Export
import Semantic.Elements.Button.ButtonOr as Export

import Semantic.Extensions.Active
import Semantic.Extensions.Animated
import Semantic.Extensions.As
import Semantic.Extensions.Attached
import Semantic.Extensions.Attributes
import Semantic.Extensions.Basic
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

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
  , click :: Ef ms IO ()
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
                      : click # (On "click" def { preventDef = True } (\_ -> return $ Just click))
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
                      : onClick click
                      : Role "button"
                      : index
                      : attributes
                      )
                      children 

instance HasAnimated (Button ms) where
    type Anim (Button ms) = Maybe Txt
    getAnimated b = animated b
    setAnimated anim b = b { animated = anim }

instance HasActive (Button ms) where
    getActive = active
    setActive a b = b { active = a }

instance HasAs (Button ms) where
    type Constructor (Button ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f b = b { as = f }

instance HasAttached (Button ms) where
    type Attach (Button ms) = Maybe Txt
    getAttached = attached
    setAttached attach b = b { attached = attach }

instance HasAttributes (Button ms) where
    type Attribute (Button ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs b = b { attributes = cs }

instance HasBasic (Button ms) where
    getBasic = basic
    setBasic bsc b = b { basic = bsc }

instance HasChildren (Button ms) where
    type Child (Button ms) = View ms
    getChildren = children
    setChildren cs b = b { children = cs }

instance HasClasses (Button ms) where
    getClasses = classes
    setClasses cs b = b { classes = cs }