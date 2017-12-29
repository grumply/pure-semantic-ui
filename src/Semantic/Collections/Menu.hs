{-# LANGUAGE UndecidableInstances #-}
module Semantic.Collections.Menu where

import GHC.Generics as G
import Pure.View hiding (active,onClick)

import Semantic.Utils

import Semantic.Collections.Menu.MenuItem

data Menu ms = Menu_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , activeIndex :: Maybe Int
    , attached :: Maybe Txt
    , borderless :: Bool
    , color :: Txt
    , compact :: Bool
    , defaultActiveIndex :: Maybe Int
    , fixed :: Txt
    , floated :: Maybe Txt
    , fluid :: Bool
    , icon :: Maybe Txt
    , inverted :: Bool
    , onItemClick :: MenuItem ms -> Ef ms IO ()
    , pagination :: Bool
    , pointing :: Bool
    , secondary :: Bool
    , size :: Txt
    , stackable :: Bool
    , tabular :: Maybe Txt
    , text :: Bool
    , vertical :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (Menu ms) where
    def = (G.to gdef) { as = Div }

pattern Menu :: VC ms => Menu ms -> View ms
pattern Menu m = View m

instance VC ms => Pure Menu ms where
    render Menu_ {..} =
        let 
            i = maybe defaultActiveIndex Just activeIndex

            children' = flip map children $ \c ->
                case c of
                    MenuItem mi -> MenuItem mi 
                        { onClick = onClick mi >> onItemClick mi
                        , active  = i == Just (index mi)
                        }
                    _ -> c
            
            cs =
                ( "ui"
                : color
                : size
                : borderless # "borderless"
                : compact # "compact"
                : fluid # "fluid"
                : inverted # "inverted"
                : pagination # "pagination"
                : pointing # "pointing"
                : secondary # "secondary"
                : stackable # "stackable"
                : text # "text"
                : vertical # "vertical"
                : may (<>> "attached") attached
                : may (<>> "floated") floated
                : may (<>> "icon") icon
                : may (<>> "tabular") tabular
                : fixed # (fixed <>> "fixed")
                : widthProp widths "item" def
                : "menu"
                : classes
                ) 
        in
            as 
                ( ClassList cs
                : attributes
                )
                children'