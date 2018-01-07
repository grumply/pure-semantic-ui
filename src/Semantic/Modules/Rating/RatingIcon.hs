{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Rating.RatingIcon where

import GHC.Generics as G
import Pure.View hiding (active,onClick,onKeyUp,Selected)

import Semantic.Utils

data RatingIcon ms = RatingIcon_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , active :: Bool
    , ariaChecked :: Bool
    , ariaPosinset :: Int
    , ariaSetsize :: Int
    , index :: Int
    , onClick :: Int -> Ef ms IO ()
    , onKeyUp :: Int -> Evt -> Ef ms IO ()
    , onMouseEnter :: Int -> Ef ms IO ()
    , selected :: Bool
    } deriving (Generic)

instance Default (RatingIcon ms) where
    def = (G.to gdef) { as = I }

pattern RatingIcon :: VC ms => RatingIcon ms -> View ms
pattern RatingIcon ri = View ri

instance VC ms => Pure RatingIcon ms where
    render RatingIcon_ {..} =
        let
            handleClick _ = 
                return $ Just (onClick index)

            handleKeyUp e@Enter = do
                prevDef e
                return $ Just (onKeyUp index e >> onClick index)
            handleKeyUp e@Space = do
                prevDef e
                return $ Just (onKeyUp index e >> onClick index)
            handleKeyUp e = return $ Just $ onKeyUp index e

            handleMouseEnter _ = 
                return $ Just (onMouseEnter index)

            cs =
                ( active # "active"
                : selected # "selected"
                : "icon"
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : Attribute "aria-checked" (ariaChecked ? "true" $ "false")
                : Attribute "aria-posinset" (toTxt ariaPosinset)
                : Attribute "aria-setsize" (toTxt ariaSetsize)
                : On "click" def handleClick
                : On "keyup" def handleKeyUp
                : On "mouseenter" def handleMouseEnter
                : Tabindex 0
                : Role "radio"
                : attributes
                )
                []
