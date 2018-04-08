{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Rating
  ( module Properties
  , module Tools
  , Rating(..), pattern Rating
  , Icon(..), pattern Icon
  ) where

import GHC.Generics as G
import Pure.View hiding (disabled,OnClick,OnMouseEnter,Selected,onKeyUp,onClick,active)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasActiveProp(..), pattern Active
  , HasIndexProp(..), pattern Index
  , HasOnClickProp(..), pattern OnClick
  , HasOnMouseEnterProp(..), pattern OnMouseEnter
  , HasSelectedProp(..), pattern Selected
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasClearableProp(..), pattern Clearable
  , HasCurrentRatingProp(..), pattern CurrentRating
  , HasDefaultRatingProp(..), pattern DefaultRating
  , HasDisabledProp(..), pattern Disabled
  , HasIsIconProp(..), pattern IsIcon
  , HasMaxRatingProp(..), pattern MaxRating
  , HasOnRateProp(..), pattern OnRate
  , HasSizeProp(..), pattern Size
  , HasOnKeyUpProp(..), pattern OnKeyUp
  )

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
                                Icon $ def
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

data Icon ms = Icon_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , active :: Bool
    , index :: Int
    , onClick :: Int -> Ef ms IO ()
    , onKeyUp :: Int -> Evt -> Ef ms IO ()
    , onMouseEnter :: Int -> Ef ms IO ()
    , selected :: Bool
    } deriving (Generic)

instance Default (Icon ms) where
    def = (G.to gdef) { as = I }

pattern Icon :: VC ms => Icon ms -> View ms
pattern Icon ri = View ri

instance VC ms => Pure Icon ms where
    render Icon_ {..} =
        let
            handleClick _ =
                let oc = onClick index
                in return $ oc # Just oc

            handleKeyUp e@Enter = do
                prevDef e
                return $ Just (onKeyUp index e >> onClick index)
            handleKeyUp e@Space = do
                prevDef e
                return $ Just (onKeyUp index e >> onClick index)
            handleKeyUp e = return $ Just $ onKeyUp index e

            handleMouseEnter _ =
                let ome = onMouseEnter index
                in return $ ome # Just ome

            cs =
                ( active # "active"
                : selected # "selected"
                : "icon"
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : On "click" def handleClick
                : On "keyup" def handleKeyUp
                : On "mouseenter" def handleMouseEnter
                : Tabindex 0
                : Role "radio"
                : attributes
                )
                []


instance HasAsProp (Icon ms) where
    type AsProp (Icon ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ri = ri { as = a }

instance HasAttributesProp (Icon ms) where
    type Attribute (Icon ms) = Feature ms
    getAttributes = attributes
    setAttributes as ri = ri { attributes = as }

instance HasClassesProp (Icon ms) where
    getClasses = classes
    setClasses cs ri = ri { classes = cs }

instance HasActiveProp (Icon ms) where
    getActive = active
    setActive a ri = ri { active = a }

instance HasIndexProp (Icon ms) where
    getIndex = index
    setIndex i ri = ri { index = i }

instance HasOnClickProp (Icon ms) where
    type OnClickProp (Icon ms) = Int -> Ef ms IO ()
    getOnClick = onClick
    setOnClick oc ri = ri { onClick = oc }

instance HasOnKeyUpProp (Icon ms) where
    type OnKeyUpProp (Icon ms) = Int -> Evt -> Ef ms IO ()
    getOnKeyUp = onKeyUp
    setOnKeyUp oku ri = ri { onKeyUp = oku }

instance HasOnMouseEnterProp (Icon ms) where
    type OnMouseEnterProp (Icon ms) = Int -> Ef ms IO ()
    getOnMouseEnter = onMouseEnter
    setOnMouseEnter ome ri = ri { onMouseEnter = ome }

instance HasSelectedProp (Icon ms) where
    getSelected = selected
    setSelected s ri = ri { selected = s }
