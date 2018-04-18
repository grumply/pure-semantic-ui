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
import Pure.Data.Txt (isInfixOf)
import Pure.View hiding (position,offset,round,trigger,OnClose,Content,Header,Offset,Styles)
import Pure.Lifted (JSV,Node(..),Element(..),(.#),window,IsJSV(..))

import Semantic.Utils hiding (on)

import Semantic.Portal hiding (PS)

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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

data Popup ms = Popup_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , basic :: Bool
    , flowing :: Bool
    , hideOnScroll :: Bool
    , hoverable :: Bool
    , inverted :: Bool
    , offset :: Double
    , onClose :: Ef ms IO ()
    , onMount :: Ef ms IO ()
    , onOpen :: Ef ms IO ()
    , onUnmount :: Ef ms IO ()
    , position :: Txt
    , size :: Txt
    , styles :: [(Txt,Txt)]
    , trigger :: View ms
    , triggerOn :: [Txt]
    , wide :: Maybe Txt
    , withPortal :: Portal ms -> Portal ms
    } deriving (Generic)

instance Default (Popup ms) where
    def = (G.to gdef)
        { as = Div
        , position = "top left"
        , triggerOn = [ "hover" ]
        , withPortal = id
        }

pattern Popup :: VC ms => Popup ms -> View ms
pattern Popup p = View p

data PopupState = PS
    { closed :: Bool
    , currentStyles :: [(Txt,Txt)]
    , currentPosition :: Txt
    , coords :: IORef (Maybe BoundingRect)
    , popupCoords :: IORef (Maybe BoundingRect)
    , scrollHandler :: IORef (IO ())
    }

instance VC ms => Pure Popup ms where
    render p =
        Component "Semantic.Modules.Popup" p $ \self ->
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
                            render d x = (d,maybe auto (pxs . round) x)

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

                        let ss = [("position","absolute"),render left l,render right r,render top t,render bottom b]

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
                        void $ parent self onClose
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
                , renderer = \Popup_ {..} PS {..} ->
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
                            : classes
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
                                        ( mergeClasses $ ClassList cs
                                        : StyleList currentStyles
                                        : HostRef handlePopupRef
                                        : attributes
                                        )
                                        children
                                    ]
                }

instance HasProp As (Popup ms) where
    type Prop As (Popup ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f p = p { as = f }

instance HasProp Attributes (Popup ms) where
    type Prop Attributes (Popup ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs p = p { attributes = cs }

instance HasProp Children (Popup ms) where
    type Prop Children (Popup ms) = [View ms]
    getProp _ = children
    setProp _ cs p = p { children = cs }

instance HasProp Classes (Popup ms) where
    type Prop Classes (Popup ms) = [Txt]
    getProp _ = classes
    setProp _ cs p = p { classes = cs }

instance HasProp Basic (Popup ms) where
    type Prop Basic (Popup ms) = Bool
    getProp _ = basic
    setProp _ b p = p { basic = b }

instance HasProp Flowing (Popup ms) where
    type Prop Flowing (Popup ms) = Bool
    getProp _ = flowing
    setProp _ f p = p { flowing = f }

instance HasProp HideOnScroll (Popup ms) where
    type Prop HideOnScroll (Popup ms) = Bool
    getProp _ = hideOnScroll
    setProp _ hos p = p { hideOnScroll = hos }

instance HasProp Hoverable (Popup ms) where
    type Prop Hoverable (Popup ms) = Bool
    getProp _ = hoverable
    setProp _ h p = p { hoverable = h }

instance HasProp Inverted (Popup ms) where
    type Prop Inverted (Popup ms) = Bool
    getProp _ = inverted
    setProp _ i p = p { inverted = i }

instance HasProp Offset (Popup ms) where
    type Prop Offset (Popup ms) = Double
    getProp _ = offset
    setProp _ o p = p { offset = o }

instance HasProp OnClose (Popup ms) where
    type Prop OnClose (Popup ms) = Ef ms IO ()
    getProp _ = onClose
    setProp _ oc p = p { onClose = oc }

instance HasProp OnMount (Popup ms) where
    type Prop OnMount (Popup ms) = Ef ms IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnOpen (Popup ms) where
    type Prop OnOpen (Popup ms) = Ef ms IO ()
    getProp _ = onOpen
    setProp _ oo p = p { onOpen = oo }

instance HasProp OnUnmount (Popup ms) where
    type Prop OnUnmount (Popup ms) = Ef ms IO ()
    getProp _ = onUnmount
    setProp _ ou p = p { onUnmount = ou }

instance HasProp Position (Popup ms) where
    type Prop Position (Popup ms) = Txt
    getProp _ = position
    setProp _ pos p = p { position = pos }

instance HasProp Size (Popup ms) where
    type Prop Size (Popup ms) = Txt
    getProp _ = size
    setProp _ sz p = p { size = sz }

instance HasProp Styles (Popup ms) where
    type Prop Styles (Popup ms) = [(Txt,Txt)]
    getProp _ = styles
    setProp _ s p = p { styles = s }

instance HasProp Trigger (Popup ms) where
    type Prop Trigger (Popup ms) = View ms
    getProp _ = trigger
    setProp _ t p = p { trigger = t }

instance HasProp TriggerOn (Popup ms) where
    type Prop TriggerOn (Popup ms) = [Txt]
    getProp _ = triggerOn
    setProp _ to p = p { triggerOn = to }

instance HasProp Wide (Popup ms) where
    type Prop Wide (Popup ms) = Maybe Txt
    getProp _ = wide
    setProp _ w p = p { wide = w }

instance HasProp WithPortal (Popup ms) where
    type Prop WithPortal (Popup ms) = Portal ms -> Portal ms
    getProp _ = withPortal
    setProp _ wp p = p { withPortal = wp }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content pc = View pc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Content ms) where
    type Prop As (Content ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f pc = pc { as = f }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs pc = pc { attributes = cs }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs pc = pc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs pc = pc { classes = cs }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header ph = View ph

instance Pure Header ms where
    render Header_ {..} =
        let
            cs =
                ( "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Header ms) where
    type Prop As (Header ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f ph = ph { as = f }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs ph = ph { attributes = cs }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs ph = ph { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs ph = ph { classes = cs }
