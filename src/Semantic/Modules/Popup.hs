{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Popup
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
import Pure.View hiding (position,offset,round,trigger,OnClose,Content,Header)
import Pure.Lifted (JSV,Node(..),Element(..),(.#),window,IsJSV(..))

import Semantic.Utils hiding (on)

import Semantic.Addons.Portal hiding (PS)

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasBasicProp(..), pattern Basic
  , HasFlowingProp(..), pattern Flowing
  , HasHideOnScrollProp(..), pattern HideOnScroll
  , HasHoverableProp(..), pattern Hoverable
  , HasInvertedProp(..), pattern Inverted
  , HasOffsetProp(..), pattern Offset
  , HasPositionProp(..), pattern Position
  , HasSizeProp(..), pattern Size
  , HasStylesProp(..), pattern Styles
  , HasTriggerOnProp(..), pattern TriggerOn
  , HasWideProp(..), pattern Wide
  , HasWithPortalProp(..), pattern WithPortal
  , HasOnMountProp(..), pattern OnMount
  , HasOnUnmountProp(..), pattern OnUnmount
  , HasOnOpenProp(..), pattern OnOpen
  , HasOnCloseProp(..), pattern OnClose
  , HasTriggerProp(..), pattern Trigger
  , HasCloseOnPortalMouseLeaveProp(..), pattern CloseOnPortalMouseLeave
  , HasCloseOnTriggerBlurProp(..), pattern CloseOnTriggerBlur
  , HasCloseOnTriggerClickProp(..), pattern CloseOnTriggerClick
  , HasCloseOnTriggerMouseLeaveProp(..), pattern CloseOnTriggerMouseLeave
  , HasCloseOnDocumentClickProp(..), pattern CloseOnDocumentClick
  , HasOpenOnTriggerClickProp(..), pattern OpenOnTriggerClick
  , HasOpenOnTriggerFocusProp(..), pattern OpenOnTriggerFocus
  , HasOpenOnTriggerMouseEnterProp(..), pattern OpenOnTriggerMouseEnter
  , HasMouseEnterDelayProp(..), pattern MouseEnterDelay
  , HasMouseLeaveDelayProp(..), pattern MouseLeaveDelay
  )

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
                            | otherwise = rightStyle

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

                        visibleTop    = topValue >= pyo
                        visibleBottom = topValue + brHeight <= pyo + ch
                        visibleLeft   = leftValue >= pyo
                        visibleRight  = leftValue + brWidth <= pxo + cw

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
                                hoverableProps = hoverable ? (CloseOnPortalMouseLeave . MouseLeaveDelay 300) $ id
                                clickProps = ("click" `elem` triggerOn) ? (OpenOnTriggerClick . CloseOnTriggerClick . CloseOnDocumentClick) $ id
                                focusProps = ("focus" `elem` triggerOn) ? (OpenOnTriggerFocus . CloseOnTriggerBlur) $ id
                                hoverProps = ("hover" `elem` triggerOn) ? (OpenOnTriggerMouseEnter . CloseOnTriggerMouseLeave . MouseLeaveDelay 70 . MouseEnterDelay 50) $ id
                            in
                                hoverProps . focusProps . clickProps . hoverableProps

                        cs =
                            ( "ui"
                            : position
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

instance HasAsProp (Popup ms) where
    type AsProp (Popup ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f p = p { as = f }

instance HasAttributesProp (Popup ms) where
    type Attribute (Popup ms) = Feature ms
    getAttributes = attributes
    setAttributes cs p = p { attributes = cs }

instance HasChildrenProp (Popup ms) where
    type Child (Popup ms) = View ms
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasClassesProp (Popup ms) where
    getClasses = classes
    setClasses cs p = p { classes = cs }

instance HasBasicProp (Popup ms) where
    getBasic = basic
    setBasic b p = p { basic = b }

instance HasFlowingProp (Popup ms) where
    getFlowing = flowing
    setFlowing f p = p { flowing = f }

instance HasHideOnScrollProp (Popup ms) where
    getHideOnScroll = hideOnScroll
    setHideOnScroll hos p = p { hideOnScroll = hos }

instance HasHoverableProp (Popup ms) where
    getHoverable = hoverable
    setHoverable h p = p { hoverable = h }

instance HasInvertedProp (Popup ms) where
    getInverted = inverted
    setInverted i p = p { inverted = i }

instance HasOffsetProp (Popup ms) where
    type OffsetProp (Popup ms) = Double
    getOffset = offset
    setOffset o p = p { offset = o }

instance HasOnCloseProp (Popup ms) where
    type OnCloseProp (Popup ms) = Ef ms IO ()
    getOnClose = onClose
    setOnClose oc p = p { onClose = oc }

instance HasOnMountProp (Popup ms) where
    type OnMountProp (Popup ms) = Ef ms IO ()
    getOnMount = onMount
    setOnMount om p = p { onMount = om }

instance HasOnOpenProp (Popup ms) where
    type OnOpenProp (Popup ms) = Ef ms IO ()
    getOnOpen = onOpen
    setOnOpen oo p = p { onOpen = oo }

instance HasOnUnmountProp (Popup ms) where
    type OnUnmountProp (Popup ms) = Ef ms IO ()
    getOnUnmount = onUnmount
    setOnUnmount ou p = p { onUnmount = ou }

instance HasPositionProp (Popup ms) where
    getPosition = position
    setPosition pos p = p { position = pos }

instance HasSizeProp (Popup ms) where
    getSize = size
    setSize sz p = p { size = sz }

instance HasStylesProp (Popup ms) where
    getStyles = styles
    setStyles s p = p { styles = s }

instance HasTriggerProp (Popup ms) where
    type TriggerProp (Popup ms) = View ms
    getTrigger = trigger
    setTrigger t p = p { trigger = t }

instance HasTriggerOnProp (Popup ms) where
    getTriggerOn = triggerOn
    setTriggerOn to p = p { triggerOn = to }

instance HasWideProp (Popup ms) where
    getWide = wide
    setWide w p = p { wide = w }

instance HasWithPortalProp (Popup ms) where
    type WithPortalProp (Popup ms) = Portal ms -> Portal ms
    getWithPortal = withPortal
    setWithPortal wp p = p { withPortal = wp }

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

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f pc = pc { as = f }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes cs pc = pc { attributes = cs }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs pc = pc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs pc = pc { classes = cs }

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

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ph = ph { as = f }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes cs ph = ph { attributes = cs }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs ph = ph { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs ph = ph { classes = cs }
