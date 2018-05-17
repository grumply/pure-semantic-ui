{-# LANGUAGE UndecidableInstances #-}
module Semantic.TextArea
  ( module Properties
  , module Tools
  , TextArea(..), pattern TextArea
  ) where

import Data.IORef
import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML
import Pure.Data.Event
import Pure.Lifted (setStyle,removeStyle,focusNode,JSV,Node(..),Element(..),(.#))
import qualified Pure.Data.Txt as T
import Data.Char (isDigit)
import Text.Read (readMaybe)

import Semantic.Utils
import qualified Semantic.Utils as Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern AutoHeight, AutoHeight(..)
  , pattern Rows, Rows(..)
  , pattern Styles, Styles(..)
  , pattern Value, Value(..)
  , pattern Focus, Focus(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data TextArea = TextArea_
    { as         :: Features -> [View] -> View
    , features :: Features
    , autoHeight :: Bool
    , rows       :: Int
    , styles     :: [(Txt,Txt)]
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
        LibraryComponentIO $ \self ->
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
                    TAS {..} <- getState self
                    mr <- liftIO (readIORef ref)
                    for_ mr $ \(Element -> r) -> do
                        removeStyle r height
                        removeStyle r "resize"

                updateHeight = do
                    TextArea_ {..} <- getProps self
                    TAS {..} <- getState self
                    autoHeight # do
                        mr <- liftIO (readIORef ref)
                        for_ mr $ \(Element -> r) -> do
                            ctas <- computedTextAreaStyles r
                            for_ ctas $ \(mh,bbw,btw) -> do
                                setStyle r height auto
                                setStyle r "overflowY" hidden
                                sh <- fromIntegral <$> scrollHeight r
                                let mh' = Prelude.round mh
                                    sh' = ceiling (sh + bbw + btw)
                                setStyle r height (pxs (max mh' sh'))
                                setStyle r "overflowY" mempty

                handleInput txt = do
                    liftIO updateHeight

                handleFocus = do
                    TAS {..} <- getState self
                    mr <- readIORef ref
                    traverse_ (focusNode . Node) mr

                handleRef (Node n) = do
                    TAS {..} <- getState self
                    writeIORef ref (Just n)
                    return Nothing

            in def
                { construct = TAS <$> newIORef def

                , mounted = updateHeight

                , receiveProps = \newprops oldstate -> do
                    oldprops <- getProps self
                    when (not (focus oldprops) && focus newprops) handleFocus
                    return oldstate

                , updated = \oldprops oldstate _ -> do
                    props <- getProps self

                    (not (autoHeight props) && autoHeight oldprops)
                        # removeAutoHeightStyles

                    ((autoHeight props && not (autoHeight oldprops)) || value oldprops /= value props)
                        # updateHeight

                , render = \TextArea_ {..} TAS {..} ->
                    as
                        : HTML.onInput handleInput
                        : HostRef handleRef
                        : HTML.Rows rows
                        : StyleList (("resize",autoHeight # "none") : styles)
                        : HTML.Value value
                        : attributes
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

instance HasProp Styles TextArea where
    type Prop Styles TextArea = [(Txt,Txt)]
    getProp _ = styles
    setProp _ ss ta = ta { styles = ss }

instance HasProp Value TextArea where
    type Prop Value TextArea = Txt
    getProp _ = value
    setProp _ v ta = ta { value = v }

instance HasProp Focus TextArea where
    type Prop Focus TextArea = Bool
    getProp _ = focus
    setProp _ f ta = ta { focus = f }
