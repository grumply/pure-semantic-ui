{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Rating (module Semantic.Modules.Rating, module Export) where

import GHC.Generics as G
import Pure.View hiding (disabled,OnClick,OnMouseEnter,Selected)

import Semantic.Utils

import Semantic.Modules.Rating.RatingIcon

import Semantic.Properties.Active
import Semantic.Properties.AriaChecked
import Semantic.Properties.AriaPosinset
import Semantic.Properties.AriaSetsize
import Semantic.Properties.Index
import Semantic.Properties.OnClick
import Semantic.Properties.OnMouseEnter
import Semantic.Properties.Selected
import Semantic.Modules.Rating.RatingIcon as Export

data Rating ms = Rating_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , clearable :: Maybe Txt
    , defaultRating :: Maybe Int
    , disabled :: Bool
    , icon :: Txt
    , maxRating :: Int
    , onRate :: Maybe Int -> Ef ms IO ()
    , rating :: Maybe Int
    , size :: Txt
    } deriving (Generic)

instance Default (Rating ms) where
    def = (G.to gdef) { as = Div, clearable = Just "auto", maxRating = 1 }

pattern Rating :: VC ms => Rating ms -> View ms
pattern Rating r = View r

data RatingState = RS
    { currentRating :: Maybe Int
    , selectedIndex :: Maybe Int
    , isSelecting :: Bool 
    }

instance VC ms => Pure Rating ms where
    render r =
        Component "Semantic.Modules.Rating" r $ \self ->
            let
                handleIconClick n = do
                    Rating_ {..} <- getProps self
                    RS {..} <- getState self
                    not disabled # do
                        let newRating = 
                                (clearable == Just "auto" && maxRating == 1) 
                                    ? (currentRating ? Nothing $ Just 1)
                                    $ (clearable == Just "" && Just n == currentRating)
                                        ? Nothing
                                        $ Just n
                        void $ setState self $ \_ RS {..} -> 
                            RS { currentRating = newRating
                               , isSelecting = False
                               , .. 
                               }
                        onRate newRating

                handleIconMouseEnter n = do
                    Rating_ {..} <- getProps self
                    not disabled #
                        void (setState self $ \_ RS {..} -> 
                                RS { selectedIndex = Just n 
                                   , isSelecting = True
                                   , ..
                                   }
                             )

                handleMouseLeave = do
                    Rating_ {..} <- getProps self
                    not disabled #
                        void (setState self $ \_ RS {..} -> 
                                RS { selectedIndex = Nothing
                                   , isSelecting = False
                                   , .. 
                                   }
                             )
                    return Nothing


            in def
                { construct = do
                    Rating_ {..} <- getProps self
                    return (RS defaultRating def def)
                , renderer = \Rating_ {..} RS {..} ->
                    let
                        cs =
                            ( "ui"
                            : icon
                            : size
                            : disabled # "disabled"
                            : (isSelecting && not disabled && selectedIndex >= Just 1) # "selected"
                            : "rating"
                            : classes
                            )
                    in
                        as
                            ( mergeClasses $ ClassList cs
                            : Role "radiogroup"
                            : On "mouseleave" def (\_ -> handleMouseLeave)
                            : attributes
                            )
                            (flip map [1..maxRating] $ \n ->
                                RatingIcon $ def 
                                    & Active (rating >= Just n) 
                                    & AriaChecked (rating == Just n) 
                                    & AriaPosinset n 
                                    & AriaSetsize maxRating 
                                    & Index n
                                    & OnClick handleIconClick
                                    & OnMouseEnter handleIconMouseEnter
                                    & Selected (selectedIndex >= Just n && isSelecting)
                            )
                }
