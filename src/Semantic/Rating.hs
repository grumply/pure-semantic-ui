{-# LANGUAGE UndecidableInstances #-}
module Semantic.Rating
  ( module Properties
  , module Tools
  , Rating(..), pattern Rating
  , Icon(..), pattern Icon
  ) where

import GHC.Generics as G
import Pure.View hiding (disabled,OnClick,OnMouseEnter,Selected,onKeyUp,onClick,active)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Active, Active(..)
  , pattern Index, Index(..)
  , pattern OnClick, OnClick(..)
  , pattern OnMouseEnter, OnMouseEnter(..)
  , pattern Selected, Selected(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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
import Pure.Data.Default as Tools

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

instance HasProp As (Rating ms) where
    type Prop As (Rating ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a r = r { as = a }

instance HasProp Attributes (Rating ms) where
    type Prop Attributes (Rating ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as r = r { attributes = as }

instance HasProp Children (Rating ms) where
    type Prop Children (Rating ms) = [View ms]
    getProp _ = children
    setProp _ cs r = r { children = cs }

instance HasProp Classes (Rating ms) where
    type Prop Classes (Rating ms) = [Txt]
    getProp _ = classes
    setProp _ cs r = r { classes = cs }

instance HasProp Clearable (Rating ms) where
    type Prop Clearable (Rating ms) = Maybe Txt
    getProp _ = clearable
    setProp _ c r = r { clearable = c }

instance HasProp DefaultRating (Rating ms) where
    type Prop DefaultRating (Rating ms) = Maybe Int
    getProp _ = defaultRating
    setProp _ dr r = r { defaultRating = dr }

instance HasProp Disabled (Rating ms) where
    type Prop Disabled (Rating ms) = Bool
    getProp _ = disabled
    setProp _ d r = r { disabled = d }

instance HasProp IsIcon (Rating ms) where
    type Prop IsIcon (Rating ms) = Txt
    getProp _ = icon
    setProp _ i r = r { icon = i }

instance HasProp MaxRating (Rating ms) where
    type Prop MaxRating (Rating ms) = Int
    getProp _ = maxRating
    setProp _ mr r = r { maxRating = mr }

instance HasProp OnRate (Rating ms) where
    type Prop OnRate (Rating ms) = Maybe Int -> Ef ms IO ()
    getProp _ = onRate
    setProp _ or r = r { onRate = or }

instance HasProp CurrentRating (Rating ms) where
    type Prop CurrentRating (Rating ms) = Maybe Int
    getProp _ = rating
    setProp _ cr r = r { rating = cr }

instance HasProp Size (Rating ms) where
    type Prop Size (Rating ms) = Txt
    getProp _ = size
    setProp _ s r = r { size = s }

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

instance HasProp As (Icon ms) where
    type Prop As (Icon ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ri = ri { as = a }

instance HasProp Attributes (Icon ms) where
    type Prop Attributes (Icon ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ri = ri { attributes = as }

instance HasProp Classes (Icon ms) where
    type Prop Classes (Icon ms) = [Txt]
    getProp _ = classes
    setProp _ cs ri = ri { classes = cs }

instance HasProp Active (Icon ms) where
    type Prop Active (Icon ms) = Bool
    getProp _ = active
    setProp _ a ri = ri { active = a }

instance HasProp Index (Icon ms) where
    type Prop Index (Icon ms) = Int
    getProp _ = index
    setProp _ i ri = ri { index = i }

instance HasProp OnClick (Icon ms) where
    type Prop OnClick (Icon ms) = Int -> Ef ms IO ()
    getProp _ = onClick
    setProp _ oc ri = ri { onClick = oc }

instance HasProp OnKeyUp (Icon ms) where
    type Prop OnKeyUp (Icon ms) = Int -> Evt -> Ef ms IO ()
    getProp _ = onKeyUp
    setProp _ oku ri = ri { onKeyUp = oku }

instance HasProp OnMouseEnter (Icon ms) where
    type Prop OnMouseEnter (Icon ms) = Int -> Ef ms IO ()
    getProp _ = onMouseEnter
    setProp _ ome ri = ri { onMouseEnter = ome }

instance HasProp Selected (Icon ms) where
    type Prop Selected (Icon ms) = Bool
    getProp _ = selected
    setProp _ s ri = ri { selected = s }
