module Semantic.Elements.List.ListItem where

import GHC.Generics as G
import Pure.View hiding (onClick)
import qualified Pure.View as HTML

import Semantic.Utils

data ListItem ms = ListItem_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Ef ms IO ()
    , value :: Txt
    } deriving (Generic)

instance Default (ListItem ms) where
    def = (G.to gdef) { as = Div }

pattern ListItem :: Typeable ms => ListItem ms -> View ms
pattern ListItem li = View li

instance Typeable ms => Pure ListItem ms where
    render ListItem_ {..} =
        let
            li = 
                case as [] [] of
                    Li _ _ -> True
                    _      -> False
            cs =
                ( active # "active"
                : disabled # "disabled"
                : (not li) # "item"
                : classes
                )

            valueProp = li ? Value value $ Prop "data-value" value
        in
            as
                ( HTML.onClick onClick
                : valueProp
                : ClassList cs
                : Role "listitem"
                : attributes
                )
                children
