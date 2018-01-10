module Semantic.Modules.Dropdown (module Semantic.Modules.Dropdown, module Export) where

import GHC.Generics as G
import Pure.View hiding (button,disabled,inline,onBlur,onClick,onFocus,simple)

import Semantic.Utils

import Semantic.Modules.Dropdown.DropdownDivider as Export
import Semantic.Modules.Dropdown.DropdownHeader as Export
import Semantic.Modules.Dropdown.DropdownItem as Export
import Semantic.Modules.Dropdown.DropdownMenu as Export
import Semantic.Modules.Dropdown.DropdownSearchInput as Export

import Prelude hiding (error)

data Dropdown ms = Dropdown_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , basic :: Bool
    , button :: Bool
    , compact :: Bool
    , disabled :: Bool
    , error :: Bool
    , fluid :: Bool
    , floating :: Bool
    , inline :: Bool
    , labeled :: Bool
    , loading :: Bool
    , item :: Bool
    , multiple :: Bool
    , onBlur :: Ef ms IO ()
    , onChange :: Ef ms IO ()
    , onClick :: Ef ms IO ()
    , onFocus :: Ef ms IO ()
    , onMouseDown :: Ef ms IO ()
    , open :: Bool
    , pointing :: Maybe Txt
    , search :: Bool
    , selection :: Bool
    , simple :: Bool
    , scrolling :: Bool
    , tabIndex :: Maybe Int
    , upward :: Bool
    } deriving (Generic)

instance Default (Dropdown ms) where
    def = (G.to gdef) { as = Div }

pattern Dropdown :: Dropdown ms -> View ms
pattern Dropdown d = View d

instance Pure Dropdown ms where
    render Dropdown_ {..} =
        let 
            cs =
                ( "ui"
                : open # "active visible"
                : disabled # "disabled"
                : error # "error"
                : loading # "loading"
                : basic # "basic"
                : button # "button"
                : compact # "compact"
                : fluid # "fluid"
                : floating # "floating"
                : inline # "inline"
                : labeled # "labeled"
                : item # "item"
                : multiple # "multiple"
                : search # "search"
                : selection # "selection"
                : simple # "simple"
                : scrolling # "scrolling"
                : upward # "upward"
                : may (<<>> "pointing") pointing
                : "dropdown"
                : classes
                )
        in as
                ( mergeClasses $ ClassList cs
                : onBlur # On "blur" def (\_ -> return $ Just onBlur)
                : onChange # On "change" def (\_ -> return $ Just onChange)
                : onClick # On "click" def (\_ -> return $ Just onClick)
                : onFocus # On "focus" def (\_ -> return $ Just onFocus)
                : onMouseDown # On "mousedown" def (\_ -> return $ Just onMouseDown)
                : may Tabindex tabIndex
                : attributes
                )
                children
