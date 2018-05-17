{-# LANGUAGE UndecidableInstances #-}
module Semantic.Popup
  ( module Properties
  , module Tools
  , Popup(..), pattern Popup
  , Content(..), pattern Content
  , Header(..), pattern Header
  ) where

import Control.Arrow ((&&&))
import Control.Concurrent
import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML
import Pure.Data.Event
import Pure.Data.Txt (isInfixOf)
import Pure.Lifted (JSV,Node(..),Element(..),(.#),window,IsJSV(..))
import Pure.DOM (onRaw)

import Semantic.Utils hiding (on)

import Semantic.Portal hiding (PS)

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Basic, Basic(..)
  , pattern Flowing, Flowing(..)
  , pattern HideOnScroll, HideOnScroll(..)
  , pattern Hoverable, Hoverable(..)
  , pattern Inverted, Inverted(..)
  , pattern Offset, Offset(..)
  , pattern Position, Position(..)
  , pattern Size, Size(..)
  , pattern Styles, Styles(..)
  , pattern TriggerOn, TriggerOn(..)
  , pattern Wide, Wide(..)
  , pattern WithPortal, WithPortal(..)
  , pattern OnMount, OnMount(..)
  , pattern OnUnmount, OnUnmount(..)
  , pattern OnOpen, OnOpen(..)
  , pattern OnClose, OnClose(..)
  , pattern Trigger, Trigger(..)
  , pattern CloseOnPortalMouseLeave, CloseOnPortalMouseLeave(..)
  , pattern CloseOnTriggerBlur, CloseOnTriggerBlur(..)
  , pattern CloseOnTriggerClick, CloseOnTriggerClick(..)
  , pattern CloseOnTriggerMouseLeave, CloseOnTriggerMouseLeave(..)
  , pattern CloseOnDocumentClick, CloseOnDocumentClick(..)
  , pattern OpenOnTriggerClick, OpenOnTriggerClick(..)
  , pattern OpenOnTriggerFocus, OpenOnTriggerFocus(..)
  , pattern OpenOnTriggerMouseEnter, OpenOnTriggerMouseEnter(..)
  , pattern MouseEnterDelay, MouseEnterDelay(..)
  , pattern MouseLeaveDelay, MouseLeaveDelay(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Popup = Popup_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , basic :: Bool
    , flowing :: Bool
    , hideOnScroll :: Bool
    , hoverable :: Bool
    , inverted :: Bool
    , offset :: Double
    , onClose :: IO ()
    , onMount :: IO ()
    , onOpen :: IO ()
    , onUnmount :: IO ()
    , position :: Txt
    , size :: Txt
    , styles :: [(Txt,Txt)]
    , trigger :: View
    , triggerOn :: [Txt]
    , wide :: Maybe Txt
    , withPortal :: Portal -> Portal
    } deriving (Generic)

instance Default Popup where
    def = (G.to gdef)
        { as = Div
        , position = "top left"
        , triggerOn = [ "hover" ]
        , withPortal = id
        }

pattern Popup :: Popup -> Popup
pattern Popup p = p

data PopupState = PS
    { closed :: Bool
    , currentStyles :: [(Txt,Txt)]
    , currentPosition :: Txt
    , coords :: IORef (Maybe BoundingRect)
    , popupCoords :: IORef (Maybe BoundingRect)
    , scrollHandler :: IORef (IO ())
    }

instance Pure Popup where
    view =
        LibraryComponentIO $ \self ->
            let
                bounds = do
                    let fi = fromIntegral :: Int -> Double
                    (fi -> pxo,fi -> pyo,fi -> cw,fi -> ch)
                        <- (,,,) <$> pageXOffset
                                 <*> pageYOffset
                                 <*> clientWidth
                                 <*> clientHeight
                    return (pxo,pyo,cw,ch)

                computePopupStyle offset pbr cbr (pxo,pyo,cw,ch) p =
                    let xOff = brWidth pbr + 8

                        isLeft   = left   `isInfixOf` p
                        isRight  = right  `isInfixOf` p
                        isTop    = top    `isInfixOf` p
                        isBottom = bottom `isInfixOf` p

                        centerV = not (isTop  || isBottom)

                        leftStyle
                            | isRight           = Nothing
                            | isLeft            = Just 0
                            | otherwise         = Just $ (brWidth cbr - brWidth pbr) / 2

                        leftStyle' = fmap (\l -> l + pxo + brLeft cbr - offset) leftStyle

                        leftStyle''
                            | centerV   = fmap (subtract xOff) leftStyle'
                            | otherwise = leftStyle'

                        rightStyle
                            | isRight   = Just 0
                            | otherwise = Nothing

                        rightStyle' = fmap (\r -> r + cw - (brRight cbr + pxo) - offset) rightStyle

                        rightStyle''
                            | centerV   = fmap (subtract xOff) rightStyle'
                            | otherwise = rightStyle'

                        topStyle
                            | isTop     = Nothing
                            | isBottom  = Just 0
                            | otherwise = Just $ negate $ (brHeight cbr + brHeight pbr) / 2

                        topStyle' = fmap (\t -> t + brBottom cbr + pyo) topStyle

                        bottomStyle
                            | isTop     = Just 0
                            | otherwise = Nothing

                        bottomStyle' = fmap (\b -> b + ch - (brTop cbr + pyo)) bottomStyle

                    in (leftStyle'',rightStyle'',topStyle',bottomStyle')

                isStyleInViewport BR {..} (pxo,pyo,cw,ch) (l,r,t,b) =
                    let
                        leftValue
                            | isJust r  = maybe 0 (\_ -> cw - fromJust r - brWidth) l
                            | otherwise = fromMaybe 0 l

                        topValue
                            | isJust b  = maybe 0 (\_ -> ch - fromJust b - brHeight) t
                            | otherwise = fromMaybe 0 t

                        visibleTop    = topValue > pyo
                        visibleBottom = topValue + brHeight < pyo + ch
                        visibleLeft   = leftValue > pyo
                        visibleRight  = leftValue + brWidth < pxo + cw

                    in visibleTop && visibleBottom && visibleLeft && visibleRight

                setPopupStyles = do
                    PS {..} <- getState self
                    Popup_ {..} <- getProps self
                    mcbr <- readIORef coords
                    mpbr <- readIORef popupCoords
                    for_ ((,) <$> mcbr <*> mpbr) $ \(cbr,pbr) -> do
                        bs  <- bounds
                        let
                            view d x = (d,maybe auto (pxs . round) x)

                            compute = computePopupStyle offset pbr cbr bs

                            s = compute position

                            positions =
                                [ "top left"
                                , "top right"
                                , "top center"
                                , "bottom left"
                                , "bottom right"
                                , "bottom center"
                                , "right center"
                                , "left center"
                                ]

                            ps = (position,s) : map (id &&& compute) (filter (/= position) positions)

                            findValid [] = (position,s)
                            findValid ((p,c) : cs)
                                | isStyleInViewport pbr bs c = (p,c)
                                | otherwise                  = findValid cs

                            (p,(l,r,t,b)) = findValid ps

                        let ss = [("position","absolute"),view left l,view right r,view top t,view bottom b]

                        setState self $ \_ PS {..} ->
                            PS { currentStyles  = ss
                               , currentPosition = p
                               , ..
                               }

                handleOpen (evtTarget -> t) = do
                    Popup_ {..} <- getProps self
                    PS {..} <- getState self
                    br <- boundingRect (Element t)
                    liftIO $ writeIORef coords (Just br)
                    onOpen

                handlePortalMount = do
                    Popup_ {..} <- getProps self
                    PS {..} <- getState self
                    sh <- liftIO $ onRaw (Node $ toJSV window) "scroll" def $ \_ _ -> liftIO $ do
                        Popup_ {..} <- getProps self
                        PS {..} <- getState self
                        setState self $ \_ PS {..} -> PS { closed = True, .. }
                        join $ readIORef scrollHandler
                        forkIO $ do
                            threadDelay 50000
                            void $ setState self $ \_ PS {..} -> PS { closed = False, .. }
                        onClose
                    liftIO $ writeIORef scrollHandler sh
                    onMount

                handlePortalUnmount = do
                    Popup_ {..} <- getProps self
                    PS {..} <- getState self
                    liftIO $ join $ readIORef scrollHandler
                    onUnmount

                handlePopupRef (Node n) = do
                    setStateIO self $ \_ PS {..} ->
                        return (PS {..},do
                            br <- boundingRect (Element n)
                            liftIO $ writeIORef popupCoords (isNull n ? Nothing $ Just br)
                            liftIO setPopupStyles
                          )
                    return Nothing

            in def
                { construct = PS def def "top left" <$> newIORef def <*> newIORef def <*> newIORef def
                , render = \Popup_ {..} PS {..} ->
                    let
                        applyPortalProps =
                            let
                                hoverableProps = hoverable ? (CloseOnPortalMouseLeave True . MouseLeaveDelay 300) $ id
                                clickProps = ("click" `elem` triggerOn) ? (OpenOnTriggerClick True . CloseOnTriggerClick True . CloseOnDocumentClick True) $ id
                                focusProps = ("focus" `elem` triggerOn) ? (OpenOnTriggerFocus True . CloseOnTriggerBlur True) $ id
                                hoverProps = ("hover" `elem` triggerOn) ? (OpenOnTriggerMouseEnter True . CloseOnTriggerMouseLeave True . MouseLeaveDelay 70 . MouseEnterDelay 50) $ id
                            in
                                hoverProps . focusProps . clickProps . hoverableProps

                        cs =
                            ( "ui"
                            : currentPosition
                            : size
                            : wide # "wide"
                            : basic # "basic"
                            : flowing # "flowing"
                            : inverted # "inverted"
                            : "popup transition visible"
                            )
                    in
                        closed
                            ? trigger
                            $ Portal $ withPortal $ applyPortalProps $ def
                                & OnClose onClose
                                & OnMount handlePortalMount
                                & OnOpen handleOpen
                                & OnUnmount handlePortalUnmount
                                & Trigger trigger
                                & Children
                                    [ as
                                        : StyleList currentStyles
                                        : HostRef handlePopupRef
                                        : attributes
                                        )
                                        children
                                    ]
                }

instance HasProp As Popup where
    type Prop As Popup = Features -> [View] -> View
    getProp _ = as
    setProp _ f p = p { as = f }

instance HasFeatures Popup where
    getFeatures = features
    setFeatures cs p = p { features = cs }

instance HasChildren Popup where
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasProp Basic Popup where
    type Prop Basic Popup = Bool
    getProp _ = basic
    setProp _ b p = p { basic = b }

instance HasProp Flowing Popup where
    type Prop Flowing Popup = Bool
    getProp _ = flowing
    setProp _ f p = p { flowing = f }

instance HasProp HideOnScroll Popup where
    type Prop HideOnScroll Popup = Bool
    getProp _ = hideOnScroll
    setProp _ hos p = p { hideOnScroll = hos }

instance HasProp Hoverable Popup where
    type Prop Hoverable Popup = Bool
    getProp _ = hoverable
    setProp _ h p = p { hoverable = h }

instance HasProp Inverted Popup where
    type Prop Inverted Popup = Bool
    getProp _ = inverted
    setProp _ i p = p { inverted = i }

instance HasProp Offset Popup where
    type Prop Offset Popup = Double
    getProp _ = offset
    setProp _ o p = p { offset = o }

instance HasProp OnClose Popup where
    type Prop OnClose Popup = IO ()
    getProp _ = onClose
    setProp _ oc p = p { onClose = oc }

instance HasProp OnMount Popup where
    type Prop OnMount Popup = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnOpen Popup where
    type Prop OnOpen Popup = IO ()
    getProp _ = onOpen
    setProp _ oo p = p { onOpen = oo }

instance HasProp OnUnmount Popup where
    type Prop OnUnmount Popup = IO ()
    getProp _ = onUnmount
    setProp _ ou p = p { onUnmount = ou }

instance HasProp Position Popup where
    type Prop Position Popup = Txt
    getProp _ = position
    setProp _ pos p = p { position = pos }

instance HasProp Size Popup where
    type Prop Size Popup = Txt
    getProp _ = size
    setProp _ sz p = p { size = sz }

instance HasProp Styles Popup where
    type Prop Styles Popup = [(Txt,Txt)]
    getProp _ = styles
    setProp _ s p = p { styles = s }

instance HasProp Trigger Popup where
    type Prop Trigger Popup = View
    getProp _ = trigger
    setProp _ t p = p { trigger = t }

instance HasProp TriggerOn Popup where
    type Prop TriggerOn Popup = [Txt]
    getProp _ = triggerOn
    setProp _ to p = p { triggerOn = to }

instance HasProp Wide Popup where
    type Prop Wide Popup = Maybe Txt
    getProp _ = wide
    setProp _ w p = p { wide = w }

instance HasProp WithPortal Popup where
    type Prop WithPortal Popup = Portal -> Portal
    getProp _ = withPortal
    setProp _ wp p = p { withPortal = wp }

data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content pc = pc

instance Pure Content where
    view Content_ {..} =
        let
            cs =
                ( "content"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ f pc = pc { as = f }

instance HasFeatures Content where
    getFeatures = features
    setFeatures cs pc = pc { features = cs }

instance HasChildren Content where
    getChildren = children
    setChildren cs pc = pc { children = cs }

data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header ph = ph

instance Pure Header where
    view Header_ {..} =
        let
            cs =
                ( "header"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ f ph = ph { as = f }

instance HasFeatures Header where
    getFeatures = features
    setFeatures cs ph = ph { features = cs }

instance HasChildren Header where
    getChildren = children
    setChildren cs ph = ph { children = cs }

