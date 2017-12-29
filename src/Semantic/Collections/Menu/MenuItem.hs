module Semantic.Collections.Menu.MenuItem where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Elements.Icon

data MenuItem ms = MenuItem_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , color :: Txt
    , disabled :: Bool
    , fitted :: Maybe Txt
    , header :: Bool
    , index :: Int
    , link :: Bool
    , name :: Txt
    , onClick :: Ef ms IO ()
    , position :: Txt
    } deriving (Generic)

instance Default (MenuItem ms) where
    def = (G.to gdef) { as = Div }

pattern MenuItem :: Typeable ms => MenuItem ms -> View ms
pattern MenuItem mi = View mi

instance Typeable ms => Pure MenuItem ms where
    render MenuItem_ {..} =
        let
            e = onClick ? A $ as

            icon = 
                case children of
                    [ Icon i ] -> True
                    _          -> False

            cs =
                ( color
                : position
                : active # "active"
                : disabled # "disabled"
                : icon # "icon"
                : header # "header"
                : link # "link"
                : fitted # "fitted"
                : classes
                )

        in
            e
                ( ClassList cs
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

