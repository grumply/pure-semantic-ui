{-# LANGUAGE UndecidableInstances #-}
module Semantic.Addons.TextArea where

import Data.IORef
import GHC.Generics as G
import Pure.View as HTML hiding (focus,value)
import Pure.Lifted (setStyle,removeStyle,focusNode,JSV,Node(..),Element(..))

import Semantic.Utils
import qualified Semantic.Utils as Utils

data TextArea ms = TextArea_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , classes :: [Txt]
    , autoHeight :: Bool
    , onChange :: Txt -> Ef ms IO ()
    , onInput :: Txt -> Ef ms IO ()
    , rows :: Int
    , styles :: [(Txt,Txt)]
    , value :: Txt
    , focus :: Bool
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
                    return $ parse cs $ \o -> do
                        mh  <- o .: "minHeight"
                        bbw <- o .: "borderBottomWidth"
                        btw <- o .: "borderTopWidth"
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
                    not autoHeight # do
                        mr <- liftIO (readIORef ref)
                        for_ mr $ \(Element -> r) -> do
                            ctas <- computedTextAreaStyles r
                            for_ ctas $ \(mh,bbw,btw) -> do
                                setStyle r height auto
                                setStyle r overflowY hidden
                                sh <- fromIntegral <$> scrollHeight r
                                setStyle r height (pxs (max (Prelude.round mh) (ceiling (sh + bbw + btw))))
                                setStyle r overflowY mempty

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
                        ( HTML.onInputChange handleChange
                        : HTML.onInput handleInput
                        : HostRef handleRef
                        : HTML.Rows rows
                        : StyleList (("resize",autoHeight # "none") : styles)
                        : HTML.Value value
                        : attributes
                        )
                        []


                }