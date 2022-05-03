{-# LANGUAGE UndecidableInstances #-}
module Semantic.TextArea
  ( module Properties
  , module Tools
  , TextArea(..), pattern TextArea
  ) where

import Pure hiding (focus,rows,max,not,(#),value)
import qualified Pure.Data.Txt as T
import Pure.Data.Lifted (setStyle,removeStyle,focusNode)

import Control.Monad
import Data.Foldable
import Data.IORef
import GHC.Generics as G
import Data.Char (isDigit)
import Text.Read (readMaybe)

import Semantic.Utils
import qualified Semantic.Utils as Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern AutoHeight, AutoHeight(..)
  , pattern Rows, Rows(..)
  , pattern Value, Value(..)
  , pattern Focus, Focus(..)
  )

import Data.Function as Tools ((&))

data TextArea = TextArea_
    { as         :: Features -> [View] -> View
    , features :: Features
    , autoHeight :: Bool
    , rows       :: Int
    , value      :: Txt
    , focus      :: Bool
    } deriving (Generic)

instance Default TextArea where
    def = (G.to gdef) { as = \fs cs -> Textarea & Features fs & Children cs, rows = 3 }

pattern TextArea :: TextArea -> TextArea
pattern TextArea ta = ta

data TextAreaState = TAS
    { ref :: IORef (Maybe JSV)
    }

instance Pure TextArea where
    view =
        Component $ \self ->
            let
                computedTextAreaStyles :: Element -> IO (Maybe (Double,Double,Double))
                computedTextAreaStyles e = do
                    cs <- computedStyles e
                    return $ do
                        let measure = (cs .#) >=> (readMaybe . fromTxt . T.takeWhile isDigit)
                        mh  <- measure "minHeight"
                        bbw <- measure "borderBottomWidth"
                        btw <- measure "borderTopWidth"
                        return (mh,bbw,btw)

                removeAutoHeightStyles = do
                    TAS {..} <- get self
                    mr <- readIORef ref
                    for_ mr $ \(Element -> r) -> do
                        removeStyle r "height"
                        removeStyle r "resize"

                updateHeight = do
                    TextArea_ {..} <- ask self
                    TAS {..} <- get self
                    autoHeight # do
                        mr <- readIORef ref
                        for_ mr $ \(Element -> r) -> do
                            ctas <- computedTextAreaStyles r
                            for_ ctas $ \(mh,bbw,btw) -> do
                                setStyle r "height" "auto"
                                setStyle r "overflowY" "hidden"
                                sh <- fromIntegral <$> scrollHeight r
                                let mh' = Prelude.round mh
                                    sh' = ceiling (sh + bbw + btw)
                                setStyle r "height" (fromIntegral (max mh' sh') px)
                                setStyle r "overflowY" mempty

                handleInput :: Txt -> IO ()
                handleInput txt = do
                    updateHeight

                handleFocus = do
                    TAS {..} <- get self
                    mr <- readIORef ref
                    traverse_ (focusNode . Node) mr

                handleRef (Node n) = do
                    TAS {..} <- get self
                    writeIORef ref (Just n)

            in def
                { construct = TAS <$> newIORef def

                , mounted = updateHeight

                , receive = \newprops oldstate -> do
                    oldprops <- ask self
                    when (not (focus oldprops) && focus newprops) handleFocus
                    return oldstate

                , updated = \oldprops oldstate -> do
                    props <- ask self

                    (not (autoHeight props) && autoHeight oldprops)
                        # removeAutoHeightStyles

                    ((autoHeight props && not (autoHeight oldprops)) || value oldprops /= value props)
                        # updateHeight

                , render = \TextArea_ {..} TAS {..} ->
                    as (features
                          & OnInput (withInput handleInput)
                          & WithHost handleRef
                          & Pure.Rows (toTxt rows)
                          & Styles [("resize",autoHeight # "none")]
                          & Pure.Value value
                       )
                       []

                }

instance HasProp As TextArea where
    type Prop As TextArea = Features -> [View] -> View
    getProp _ = as
    setProp _ f ta = ta { as = f }

instance HasFeatures TextArea where
    getFeatures = features
    setFeatures cs ta = ta { features = cs }

instance HasProp AutoHeight TextArea where
    type Prop AutoHeight TextArea = Bool
    getProp _ = autoHeight
    setProp _ ah ta = ta { autoHeight = ah }

instance HasProp Rows TextArea where
    type Prop Rows TextArea = Int
    getProp _ = rows
    setProp _ r ta = ta { rows = r }

instance HasProp Value TextArea where
    type Prop Value TextArea = Txt
    getProp _ = value
    setProp _ v ta = ta { value = v }

instance HasProp Focus TextArea where
    type Prop Focus TextArea = Bool
    getProp _ = focus
    setProp _ f ta = ta { focus = f }
