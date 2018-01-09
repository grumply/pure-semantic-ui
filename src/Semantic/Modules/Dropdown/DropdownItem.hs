module Semantic.Modules.Dropdown.DropdownItem where

import GHC.Generics as G
import Pure.View hiding (disabled,onClick)

import Semantic.Utils

data DropdownItem item ms = DropdownItem_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Maybe item -> Ef ms IO ()
    , selected :: Bool
    , item :: Maybe item
    , renderDropdownItem :: Maybe item -> [View ms]
    } deriving (Generic)

instance Default (DropdownItem item ms) where
    def = (G.to gdef) { as = Div }

pattern DropdownItem :: Typeable item => DropdownItem item ms -> View ms
pattern DropdownItem di = View di

instance Typeable item => Pure (DropdownItem item) ms where
    render di@DropdownItem_ {..} =
        let
            cs = 
                ( active # "active"
                : disabled # "disabled"
                : selected # "selected"
                : "item"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : On "click" def (\_ -> return $ Just $ onClick item)
                : Role "option"
                : Attribute "aria-disabled" (disabled ? "true" $ "false")
                : Attribute "aria-active" (active ? "true" $ "false")
                : Attribute "aria-selected" (selected ? "true" $ "false")
                : attributes
                )
                (renderDropdownItem item)
