module Semantic.Elements.Step where

import GHC.Generics as G
import Pure.View hiding (color,disabled,horizontal,textAlign,vertical)

import Semantic.Utils

data Step ms = Step_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , completed :: Bool
    , disabled :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    , ordered :: Bool
    } deriving (Generic)

instance Default (Step ms) where
    def = (G.to gdef) { as = Div }

pattern Step :: Typeable ms => Step ms -> View ms
pattern Step s = View s

instance Typeable ms => Pure Step ms where
    render Step_ {..} =
        let
            e = onClick ? A $ as

            cs =
                ( active # "active"
                : completed # "completed"
                : disabled # "disabled"
                : link # "link"
                : "step"
                : classes
                )
        in
            e
                ( ClassList cs 
                : ref
                : onClick # (disabled #! On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children