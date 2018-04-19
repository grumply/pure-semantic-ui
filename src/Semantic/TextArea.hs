{-# LANGUAGE UndecidableInstances #-}
module Semantic.TextArea
  ( module Properties
  , module Tools
  , TextArea(..), pattern TextArea
  ) where

import Data.IORef
import GHC.Generics as G
import Pure.View hiding (focus,value,onInput,Styles,Value)
import qualified Pure.View as HTML
import Pure.Lifted (setStyle,removeStyle,focusNode,JSV,Node(..),Element(..),(.#))
import qualified Pure.Data.Txt as T
import Data.Char (isDigit)
import Text.Read (readMaybe)

import Semantic.Utils
import qualified Semantic.Utils as Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Classes, Classes(..)
  , pattern AutoHeight, AutoHeight(..)
  , pattern OnChange, OnChange(..)
  , pattern OnInput, OnInput(..)
  , pattern Rows, Rows(..)
  , pattern Styles, Styles(..)
  , pattern Value, Value(..)
  , pattern Focus, Focus(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data TextArea ms = TextArea_
    { as         :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes    :: [Txt]
    , autoHeight :: Bool
    , onChange   :: Txt -> Ef ms IO ()
    , onInput    :: Txt -> Ef ms IO ()
    , rows       :: Int
    , styles     :: [(Txt,Txt)]
    , value      :: Txt
    , focus      :: Bool
    } deriving (Generic)

instance Default (TextArea ms) where
    def = (G.to gdef) { as = Textarea, rows = 3 }

pattern TextArea :: VC ms => TextArea ms -> View ms
pattern TextArea ta = View ta

data TextAreaState = TAS
    { ref :: IORef (Maybe JSV)
    }

instance VC ms => Pure TextArea ms where
    render ta =
        Component "Semantic.Addons.TextArea" ta $ \self ->
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

                handleChange txt = do
                    TextArea_ {..} <- getProps self
                    onChange txt

                handleInput txt = do
                    TextArea_ {..} <- getProps self
                    onInput txt
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

                , renderer = \TextArea_ {..} TAS {..} ->
                    as
                        ( mergeClasses $ ClassList classes
                        : HTML.onInputChange handleChange
                        : HTML.onInput handleInput
                        : HostRef handleRef
                        : HTML.Rows rows
                        : StyleList (("resize",autoHeight # "none") : styles)
                        : HTML.Value value
                        : attributes
                        )
                        []

                }

instance HasProp As (TextArea ms) where
    type Prop As (TextArea ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f ta = ta { as = f }

instance HasProp Attributes (TextArea ms) where
    type Prop Attributes (TextArea ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs ta = ta { attributes = cs }

instance HasProp Classes (TextArea ms) where
    type Prop Classes (TextArea ms) = [Txt]
    getProp _ = classes
    setProp _ cs ta = ta { classes = cs }

instance HasProp AutoHeight (TextArea ms) where
    type Prop AutoHeight (TextArea ms) = Bool
    getProp _ = autoHeight
    setProp _ ah ta = ta { autoHeight = ah }

instance HasProp OnChange (TextArea ms) where
    type Prop OnChange (TextArea ms) = Txt -> Ef ms IO ()
    getProp _ = onChange
    setProp _ oc ta = ta { onChange = oc }

instance HasProp OnInput (TextArea ms) where
    type Prop OnInput (TextArea ms) = Txt -> Ef ms IO ()
    getProp _ = onInput
    setProp _ oi ta = ta { onInput = oi }

instance HasProp Rows (TextArea ms) where
    type Prop Rows (TextArea ms) = Int
    getProp _ = rows
    setProp _ r ta = ta { rows = r }

instance HasProp Styles (TextArea ms) where
    type Prop Styles (TextArea ms) = [(Txt,Txt)]
    getProp _ = styles
    setProp _ ss ta = ta { styles = ss }

instance HasProp Value (TextArea ms) where
    type Prop Value (TextArea ms) = Txt
    getProp _ = value
    setProp _ v ta = ta { value = v }

instance HasProp Focus (TextArea ms) where
    type Prop Focus (TextArea ms) = Bool
    getProp _ = focus
    setProp _ f ta = ta { focus = f }
