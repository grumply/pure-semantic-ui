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

data WithPopup = WithPopup_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , popup :: [View]
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
    , triggerOn :: [Txt]
    , wide :: Maybe Txt
    } deriving (Generic)

instance Default WithPopup where
    def = (G.to gdef)
        { as = Div
        , position = "top left"
        , triggerOn = [ "hover" ]
        , withPortal = id
        }

pattern WithPopup :: WithPopup -> WithPopup
pattern WithPopup p = p

data WithPopupState = WPS
    { closed :: Bool
    , currentStyles :: [(Txt,Txt)]
    , currentPosition :: Txt
    , coords :: IORef (Maybe BoundingRect)
    , popupCoords :: IORef (Maybe BoundingRect)
    , scrollHandler :: IORef (IO ())
    }

instance Pure WithPopup where
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
                    WPS {..} <- getState self
                    WithPopup_ {..} <- getProps self
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

                        setState self $ \_ WPS {..} ->
                            WPS { currentStyles  = ss
                               , currentPosition = p
                               , ..
                               }

                handleOpen (evtTarget -> t) = do
                    WithPopup_ {..} <- getProps self
                    WPS {..} <- getState self
                    br <- boundingRect (Element t)
                    liftIO $ writeIORef coords (Just br)
                    onOpen

                handlePortalMount = do
                    WithPopup_ {..} <- getProps self
                    WPS {..} <- getState self
                    sh <- liftIO $ onRaw (Node $ toJSV window) "scroll" def $ \_ _ -> liftIO $ do
                        WithPopup_ {..} <- getProps self
                        WPS {..} <- getState self
                        setState self $ \_ WPS {..} -> WPS { closed = True, .. }
                        join $ readIORef scrollHandler
                        forkIO $ do
                            threadDelay 50000
                            void $ setState self $ \_ WPS {..} -> WPS { closed = False, .. }
                        onClose
                    liftIO $ writeIORef scrollHandler sh
                    onMount

                handlePortalUnmount = do
                    WithPopup_ {..} <- getProps self
                    WPS {..} <- getState self
                    liftIO $ join $ readIORef scrollHandler
                    onUnmount

                handlePopupRef (Node n) = do
                    setStateIO self $ \_ WPS {..} ->
                        return (WPS {..},do
                            br <- boundingRect (Element n)
                            liftIO $ writeIORef popupCoords (isNull n ? Nothing $ Just br)
                            liftIO setPopupStyles
                          )
                    return Nothing

            in def
                { construct = WPS def def "top left" <$> newIORef def <*> newIORef def <*> newIORef def
                , render = \WithPopup_ {..} WPS {..} ->
                    let
                        popupProps =
                            let
                                hoverableProps = hoverable ? (CloseOnPortalMouseLeave True . MouseLeaveDelay 300) $ id
                                clickProps = ("click" `elem` triggerOn) ? (OpenOnTriggerClick True . CloseOnTriggerClick True . CloseOnDocumentClick True) $ id
                                focusProps = ("focus" `elem` triggerOn) ? (OpenOnTriggerFocus True . CloseOnTriggerBlur True) $ id
                                hoverProps = ("hover" `elem` triggerOn) ? (OpenOnTriggerMouseEnter True . CloseOnTriggerMouseLeave True . MouseLeaveDelay 70 . MouseEnterDelay 50) $ id
                            in
                                hoverProps . focusProps . clickProps . hoverableProps

                        cs =
                            [ "ui"
                            , currentPosition
                            , size
                            , wide # "wide"
                            , basic # "basic"
                            , flowing # "flowing"
                            , inverted # "inverted"
                            , "popup transition visible"
                            ]
                    in
                        as feature (Portal Pure.Data.Lifted.body (Popup def <| popup popupProps) : children)
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

instance HasProp As WithPopup where
    type Prop As WithPopup = Features -> [View] -> View
    getProp _ = as
    setProp _ f p = p { as = f }

instance HasFeatures WithPopup where
    getFeatures = features
    setFeatures cs p = p { features = cs }

instance HasChildren WithPopup where
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasProp Basic WithPopup where
    type Prop Basic WithPopup = Bool
    getProp _ = basic
    setProp _ b p = p { basic = b }

instance HasProp Flowing WithPopup where
    type Prop Flowing WithPopup = Bool
    getProp _ = flowing
    setProp _ f p = p { flowing = f }

instance HasProp HideOnScroll WithPopup where
    type Prop HideOnScroll WithPopup = Bool
    getProp _ = hideOnScroll
    setProp _ hos p = p { hideOnScroll = hos }

instance HasProp Hoverable WithPopup where
    type Prop Hoverable WithPopup = Bool
    getProp _ = hoverable
    setProp _ h p = p { hoverable = h }

instance HasProp Inverted WithPopup where
    type Prop Inverted WithPopup = Bool
    getProp _ = inverted
    setProp _ i p = p { inverted = i }

instance HasProp Offset WithPopup where
    type Prop Offset WithPopup = Double
    getProp _ = offset
    setProp _ o p = p { offset = o }

instance HasProp OnClose WithPopup where
    type Prop OnClose WithPopup = IO ()
    getProp _ = onClose
    setProp _ oc p = p { onClose = oc }

instance HasProp OnMount WithPopup where
    type Prop OnMount WithPopup = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnOpen WithPopup where
    type Prop OnOpen WithPopup = IO ()
    getProp _ = onOpen
    setProp _ oo p = p { onOpen = oo }

instance HasProp OnUnmount WithPopup where
    type Prop OnUnmount WithPopup = IO ()
    getProp _ = onUnmount
    setProp _ ou p = p { onUnmount = ou }

instance HasProp Position WithPopup where
    type Prop Position WithPopup = Txt
    getProp _ = position
    setProp _ pos p = p { position = pos }

instance HasProp Size WithPopup where
    type Prop Size WithPopup = Txt
    getProp _ = size
    setProp _ sz p = p { size = sz }

instance HasProp Styles WithPopup where
    type Prop Styles WithPopup = [(Txt,Txt)]
    getProp _ = styles
    setProp _ s p = p { styles = s }

instance HasProp Trigger WithPopup where
    type Prop Trigger WithPopup = View
    getProp _ = trigger
    setProp _ t p = p { trigger = t }

instance HasProp TriggerOn WithPopup where
    type Prop TriggerOn WithPopup = [Txt]
    getProp _ = triggerOn
    setProp _ to p = p { triggerOn = to }

instance HasProp Wide WithPopup where
    type Prop Wide WithPopup = Maybe Txt
    getProp _ = wide
    setProp _ w p = p { wide = w }

instance HasProp WithPortal WithPopup where
    type Prop WithPortal WithPopup = Portal -> Portal
    getProp _ = withPortal
    setProp _ wp p = p { withPortal = wp }

data Popup = Popup_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Popup where
  def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Popup :: Popup -> Popup
pattern Popup p = p

instance HasProp As Popup where
    type Prop As Popup = Features -> [View] -> View
    getProp _ = as
    setProp _ a p = p { as = a }

instance HasFeatures Popup where
    getFeatures = features
    setFeatures fs p = p { features = fs }

instance HasChildren Popup where
    getChildren = children
    setChildren cs p = p { children = cs }

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

