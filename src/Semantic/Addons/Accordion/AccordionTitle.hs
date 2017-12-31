module Semantic.Addons.Accordion.AccordionTitle where

import GHC.Generics as G
import Pure.View hiding (active,onClick)

import Semantic.Utils

data AccordionTitle ms = AccordionTitle_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , index :: Int
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (AccordionTitle ms) where
    def = (G.to gdef) { as = Div }

pattern AccordionTitle :: Typeable ms => AccordionTitle ms -> View ms
pattern AccordionTitle at = View at

instance Typeable ms => Pure AccordionTitle ms where
    render AccordionTitle_ {..} =
        let
            cs =
                ( active # "active"
                : "title"
                : classes
                )
        in
            as
                ( ClassList cs
                : onClick # (On "click" def $ \_ -> return (Just onClick))
                : attributes
                )
                children
