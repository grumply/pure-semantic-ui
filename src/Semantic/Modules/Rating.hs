{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Rating (module Semantic.Modules.Rating, module Export) where

import GHC.Generics as G
import Pure.View hiding (disabled,OnClick,OnMouseEnter,Selected)

import Semantic.Utils

import Semantic.Modules.Rating.RatingIcon

import Semantic.Properties.Active
import Semantic.Properties.Index
import Semantic.Properties.OnClick
import Semantic.Properties.OnMouseEnter
import Semantic.Properties.Selected

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Clearable
import Semantic.Properties.CurrentRating
import Semantic.Properties.DefaultRating
import Semantic.Properties.Disabled
import Semantic.Properties.IsIcon
import Semantic.Properties.MaxRating
import Semantic.Properties.OnRate
import Semantic.Properties.Size

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
                                    & Index n
                                    & OnClick handleIconClick
                                    & OnMouseEnter handleIconMouseEnter
                                    & Selected (selectedIndex >= Just n && isSelecting)
                            )
                }

instance HasAsProp (Rating ms) where
    type AsProp (Rating ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a r = r { as = a }

instance HasAttributesProp (Rating ms) where
    type Attribute (Rating ms) = Feature ms
    getAttributes = attributes
    setAttributes as r = r { attributes = as }

instance HasChildrenProp (Rating ms) where
    type Child (Rating ms) = View ms
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasClassesProp (Rating ms) where
    getClasses = classes
    setClasses cs r = r { classes = cs }

instance HasClearableProp (Rating ms) where
    getClearable = clearable
    setClearable c r = r { clearable = c }

instance HasDefaultRatingProp (Rating ms) where
    getDefaultRating = defaultRating
    setDefaultRating dr r = r { defaultRating = dr }

instance HasDisabledProp (Rating ms) where
    getDisabled = disabled
    setDisabled d r = r { disabled = d }

instance HasIsIconProp (Rating ms) where
    type IsIconProp (Rating ms) = Txt
    getIsIcon = icon
    setIsIcon i r = r { icon = i }

instance HasMaxRatingProp (Rating ms) where
    getMaxRating = maxRating
    setMaxRating mr r = r { maxRating = mr }

instance HasOnRateProp (Rating ms) where
    type OnRateProp (Rating ms) = Maybe Int -> Ef ms IO ()
    getOnRate = onRate
    setOnRate or r = r { onRate = or }

instance HasCurrentRatingProp (Rating ms) where
    getCurrentRating = rating
    setCurrentRating cr r = r { rating = cr }

instance HasSizeProp (Rating ms) where
    getSize = size
    setSize s r = r { size = s }