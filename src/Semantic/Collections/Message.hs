module Semantic.Collections.Message where

import GHC.Generics as G
import Pure.View hiding (Name)

import Semantic.Utils

import Semantic.Elements.Icon

import Semantic.Properties.Name
import Semantic.Properties.Attributes

data Message ms = Message_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , color :: Txt
    , compact :: Bool
    , error :: Bool
    , floating :: Bool
    , hidden :: Bool
    , info :: Bool
    , negative :: Bool
    , onDismiss :: Ef ms IO ()
    , positive :: Bool
    , size :: Txt
    , success :: Bool
    , visible :: Bool
    , warning :: Bool
    } deriving (Generic)

instance Default (Message ms) where
    def = (G.to gdef) { as = Div }

pattern Message :: Typeable ms => Message ms -> View ms
pattern Message m = View m

instance Typeable ms => Pure Message ms where
    render Message_ {..} =
        let
            icon = foldPures (\(Icon_ {}) -> const True) False children

            dismissIcon = onDismiss # (Icon $ def & Name "close" & Attributes [ On "click" def (\_ -> return $ Just onDismiss) ])

            cs =
                ( "ui"
                : color
                : size
                : compact # "compact"
                : error # "error"
                : floating # "floating"
                : hidden # "hidden"
                : icon # "icon"
                : info # "info"
                : negative # "negative"
                : positive # "positive"
                : success # "success"
                : visible # "visible"
                : warning # "warning"
                : may (<>> "attached") attached 
                : "message"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                ( dismissIcon : children )