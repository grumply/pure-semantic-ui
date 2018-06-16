{-# LANGUAGE UndecidableInstances #-}
module Semantic.Rating
  ( module Properties
  , module Tools
  , Rating(..), pattern Rating
  , Icon(..), pattern Semantic.Rating.Icon
  ) where

import Pure

import Control.Monad
import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern Index, Index(..)
  , pattern Selected, Selected(..)
  , pattern As, As(..)
  , pattern Clearable, Clearable(..)
  , pattern CurrentRating, CurrentRating(..)
  , pattern DefaultRating, DefaultRating(..)
  , pattern Disabled, Disabled(..)
  , pattern IsIcon, IsIcon(..)
  , pattern MaxRating, MaxRating(..)
  , pattern OnRate, OnRate(..)
  , pattern Size, Size(..)
  , pattern OnKeyUp, OnKeyUp(..)
  )

import Data.Function as Tools ((&))

data Rating = Rating_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , clearable :: Maybe Txt
    , defaultRating :: Maybe Int
    , disabled :: Bool
    , icon :: Txt
    , maxRating :: Int
    , onRate :: Maybe Int -> IO ()
    , rating :: Maybe Int
    , size :: Txt
    } deriving (Generic)

instance Default Rating where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs, clearable = Just "auto", maxRating = 1 }

pattern Rating :: Rating -> Rating
pattern Rating r = r

data RatingState = RS
    { currentRating :: Maybe Int
    , selectedIndex :: Maybe Int
    , isSelecting :: Bool
    }

instance Pure Rating where
    view =
        LibraryComponentIO $ \self ->
            let
                handleIconClick n _ = do
                    Rating_ {..} <- ask self
                    RS {..} <- get self
                    not disabled # do
                        let newRating =
                                (clearable == Just "auto" && maxRating == 1)
                                    ? ((currentRating /= Nothing) ? Nothing $ Just 1)
                                    $ (clearable == Just "" && Just n == currentRating)
                                        ? Nothing
                                        $ Just n
                        modify_ self $ \_ RS {..} ->
                            RS { currentRating = newRating
                               , isSelecting = False
                               , ..
                               }
                        onRate newRating

                handleIconMouseEnter n _ = do
                    Rating_ {..} <- ask self
                    not disabled #
                        modify_ self (\_ RS {..} ->
                            RS { selectedIndex = Just n
                               , isSelecting = True
                               , ..
                               })

                handleMouseLeave = do
                    Rating_ {..} <- ask self
                    not disabled #
                        modify_ self (\_ RS {..} ->
                            RS { selectedIndex = Nothing
                                , isSelecting = False
                                , ..
                                })

            in def
                { construct = do
                    Rating_ {..} <- ask self
                    return (RS defaultRating def def)
                , render = \Rating_ {..} RS {..} ->
                    let
                        cs =
                            [ "ui"
                            , icon
                            , size
                            , disabled # "disabled"
                            , (isSelecting && not disabled && selectedIndex >= Just 1) # "selected"
                            , "rating"
                            ]
                    in
                        as (features & Classes cs & Role "radiogroup" & OnMouseLeave (const handleMouseLeave))
                            (flip fmap [1..maxRating] $ \n ->
                                View $ Semantic.Rating.Icon $ def
                                    & Active (rating >= Just n)
                                    & Index n
                                    & OnClick (handleIconClick n)
                                    & OnMouseEnter (handleIconMouseEnter n)
                                    & Pure.Selected (toTxt $ selectedIndex >= Just n && isSelecting)
                            )
                }

instance HasProp As Rating where
    type Prop As Rating = Features -> [View] -> View
    getProp _ = as
    setProp _ a r = r { as = a }

instance HasFeatures Rating where
    getFeatures = features
    setFeatures as r = r { features = as }

instance HasChildren Rating where
    getChildren = children
    setChildren cs r = r { children = cs }

instance HasProp Clearable Rating where
    type Prop Clearable Rating = Maybe Txt
    getProp _ = clearable
    setProp _ c r = r { clearable = c }

instance HasProp DefaultRating Rating where
    type Prop DefaultRating Rating = Maybe Int
    getProp _ = defaultRating
    setProp _ dr r = r { defaultRating = dr }

instance HasProp Disabled Rating where
    type Prop Disabled Rating = Bool
    getProp _ = disabled
    setProp _ d r = r { disabled = d }

instance HasProp IsIcon Rating where
    type Prop IsIcon Rating = Txt
    getProp _ = icon
    setProp _ i r = r { icon = i }

instance HasProp MaxRating Rating where
    type Prop MaxRating Rating = Int
    getProp _ = maxRating
    setProp _ mr r = r { maxRating = mr }

instance HasProp OnRate Rating where
    type Prop OnRate Rating = Maybe Int -> IO ()
    getProp _ = onRate
    setProp _ or r = r { onRate = or }

instance HasProp CurrentRating Rating where
    type Prop CurrentRating Rating = Maybe Int
    getProp _ = rating
    setProp _ cr r = r { rating = cr }

instance HasProp Size Rating where
    type Prop Size Rating = Txt
    getProp _ = size
    setProp _ s r = r { size = s }

data Icon = Icon_
    { as :: Features -> [View] -> View
    , features :: Features
    , active :: Bool
    , index :: Int
    , onClick :: Int -> IO ()
    , onKeyUp :: Int -> Evt -> IO ()
    , onMouseEnter :: Int -> IO ()
    , selected :: Bool
    } deriving (Generic)

instance Default Icon where
    def = (G.to gdef) { as = \fs cs -> I & Features fs & Children cs }

pattern Icon :: Icon -> Icon
pattern Icon ri = ri

instance Pure Icon where
    view Icon_ {..} =
        let
            handleClick _ = onClick index

            handleKeyUp e@Enter = do
              prevDef e
              onKeyUp index e
              onClick index
            handleKeyUp e@Space = do
              prevDef e
              onKeyUp index e
              onClick index
            handleKeyUp e =
              onKeyUp index e

            handleMouseEnter _ = onMouseEnter index

            cs =
                [ active # "active"
                , selected # "selected"
                , "icon"
                ]

        in
            as (features & Classes cs & OnClick handleClick & Pure.OnKeyUp handleKeyUp & OnMouseEnter handleMouseEnter & TabIndex "0" & Role "radio")
               []

instance HasProp As Icon where
    type Prop As Icon = Features -> [View] -> View
    getProp _ = as
    setProp _ a ri = ri { as = a }

instance HasFeatures Icon where
    getFeatures = features
    setFeatures as ri = ri { features = as }

instance HasProp Active Icon where
    type Prop Active Icon = Bool
    getProp _ = active
    setProp _ a ri = ri { active = a }

instance HasProp Index Icon where
    type Prop Index Icon = Int
    getProp _ = index
    setProp _ i ri = ri { index = i }

instance HasProp Selected Icon where
    type Prop Selected Icon = Bool
    getProp _ = selected
    setProp _ s ri = ri { selected = s }
