{-# LANGUAGE UndecidableInstances #-}
module Semantic.Addons.TextArea where

import Data.IORef
import GHC.Generics as G
import Pure.View hiding (focus,value,onInput)
import qualified Pure.View as HTML
import Pure.Lifted (setStyle,removeStyle,focusNode,JSV,Node(..),Element(..),(.#))
import qualified Pure.Data.Txt as T
import Data.Char (isDigit)
import Text.Read (readMaybe)

import Semantic.Utils
import qualified Semantic.Utils as Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Classes
import Semantic.Properties.AutoHeight
import Semantic.Properties.OnChange
import Semantic.Properties.OnInput
import Semantic.Properties.Rows
import Semantic.Properties.Styles
import Semantic.Properties.Value
import Semantic.Properties.Focus

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

instance HasAsProp (TextArea ms) where
    type AsProp (TextArea ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ta = ta { as = f }

instance HasAttributesProp (TextArea ms) where
    type Attribute (TextArea ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ta = ta { attributes = cs }

instance HasClassesProp (TextArea ms) where
    getClasses = classes
    setClasses cs ta = ta { classes = cs }

instance HasAutoHeightProp (TextArea ms) where
    getAutoHeight = autoHeight
    setAutoHeight ah ta = ta { autoHeight = ah }

instance HasOnChangeProp (TextArea ms) where
    type OnChangeProp (TextArea ms) = Txt -> Ef ms IO ()
    getOnChange = onChange
    setOnChange oc ta = ta { onChange = oc }

instance HasOnInputProp (TextArea ms) where
    type OnInputProp (TextArea ms) = Txt -> Ef ms IO ()
    getOnInput = onInput
    setOnInput oi ta = ta { onInput = oi }

instance HasRowsProp (TextArea ms) where
    getRows = rows
    setRows r ta = ta { rows = r }

instance HasStylesProp (TextArea ms) where
    getStyles = styles
    setStyles ss ta = ta { styles = ss }

instance HasValueProp (TextArea ms) where
    getValue = value
    setValue v ta = ta { value = v }

instance HasFocusProp (TextArea ms) where
    getFocus = focus
    setFocus f ta = ta { focus = f }