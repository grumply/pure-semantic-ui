module Semantic.Addons.Checkbox where

import GHC.Generics as G
import Pure.View hiding (disabled,name,onClick,Checked,Name,Type,Value)
import qualified Pure.View as HTML
import Pure.Lifted (Node)

import Semantic.Utils

data Checkbox ms = Checkbox_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , checked :: Checked
    , disabled :: Bool
    , fitted :: Bool
    , name :: Txt
    , onChange :: Checkbox ms -> Ef ms IO ()
    , onClick :: Checkbox ms -> Ef ms IO ()
    , onMouseDown :: Checkbox ms -> Ef ms IO ()
    , withRef :: Node -> Ef ms IO ()
    , radio :: Bool
    , readOnly :: Bool
    , slider :: Bool
    , tabIndex :: Maybe Int
    , toggle :: Bool
    , _type :: Txt
    , value :: Txt
    } deriving (Generic)

instance Default (Checkbox ms) where
    def = (G.to gdef) { as = Div }

pattern Checkbox :: Typeable ms => Checkbox ms -> View ms
pattern Checkbox c = View c

renderChecked Checked = HTML.Checked True
renderChecked Indeterminate = HTML.Checked False
renderChecked Unchecked = nil

instance Typeable ms => Pure Checkbox ms where
    render cb@Checkbox_ {..} =
        let
            cs =
                ( "ui"
                : (checked == Checked) # "checked"
                : disabled # "disabled"
                : (checked == Indeterminate) # "indeterminate"
                : fitted # "fitted" 
                : radio # "radio"
                : readOnly # "read-only"
                : slider # "slider"
                : toggle # "toggle"
                : "checkbox"
                : classes
                )

        in
            as
                ( ClassList cs
                : On "change" def (\_ -> return $ Just (onChange cb))
                : On "click" def (\_ -> return $ Just (onClick cb))
                : On "mousedown" def (\_ -> return $ Just (onMouseDown cb))
                : attributes
                )
                ( HTML.Input
                    [ ClassList [ "hidden" ]
                    , HostRef (return . Just . withRef)
                    , renderChecked checked
                    , HTML.Name name
                    , Readonly readOnly
                    , may (\ti -> Tabindex (disabled ? (-1) $ ti)) tabIndex
                    , HTML.Type _type
                    , HTML.Value value
                    ]
                    []
                : children
                )
