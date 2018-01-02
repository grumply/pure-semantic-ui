{-# LANGUAGE ScopedTypeVariables #-}
module Semantic.Addons.Portal where

import Control.Concurrent
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import GHC.Generics as G
import Pure.Data.Txt as Txt (unwords)
import Pure.View hiding (active,trigger,Proxy)
import Pure.Lifted (getDocument,append,getChild,removeNode,setProperty,create,insertAt,JSV,Node(..),Element(..),Doc(..))
import Pure.DOM

import Semantic.Utils

import Semantic.Addons.Proxy

import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.CloseOnDocumentClick
import Semantic.Properties.CloseOnEscape
import Semantic.Properties.CloseOnPortalMouseLeave
import Semantic.Properties.CloseOnRootNodeClick
import Semantic.Properties.CloseOnTriggerBlur
import Semantic.Properties.CloseOnTriggerMouseLeave
import Semantic.Properties.DefaultOpen
import Semantic.Properties.MountNode
import Semantic.Properties.MouseEnterDelay
import Semantic.Properties.MouseLeaveDelay
import Semantic.Properties.OnClose
import Semantic.Properties.OnMount
import Semantic.Properties.OnOpen
import Semantic.Properties.OnUnmount
import Semantic.Properties.Open
import Semantic.Properties.OpenOnTriggerClick
import Semantic.Properties.OpenOnTriggerFocus
import Semantic.Properties.OpenOnTriggerMouseEnter
import Semantic.Properties.Prepend
import Semantic.Properties.Trigger
import Semantic.Properties.InnerRef

import Unsafe.Coerce

data Portal ms = Portal_
    { attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , closeOnDocumentClick :: Bool
    , closeOnEscape :: Bool
    , closeOnPortalMouseLeave :: Bool
    , closeOnRootNodeClick :: Bool
    , closeOnTriggerBlur :: Bool
    , closeOnTriggerClick :: Bool
    , closeOnTriggerMouseLeave :: Bool
    , defaultOpen :: Bool
    , mountNode :: Maybe JSV
    , mouseEnterDelay :: Int
    , mouseLeaveDelay :: Int
    , onClose :: Ef ms IO ()
    , onMount :: Ef ms IO ()
    , onOpen :: Ef ms IO ()
    , onUnmount :: Ef ms IO ()
    , open :: Bool
    , openOnTriggerClick :: Bool
    , openOnTriggerFocus :: Bool
    , openOnTriggerMouseEnter :: Bool
    , prepend :: Bool
    , trigger :: View ms
    } deriving (Generic)

instance Default (Portal ms) where
    def = (G.to gdef) 
            { closeOnDocumentClick = True
            , closeOnEscape = True
            , openOnTriggerClick = True 
            }

pattern Portal :: Typeable ms => Portal ms -> View ms
pattern Portal p = View p

data PortalState = PS
    { active :: Bool
    , nodes :: IORef PortalStateNodes
    , timers :: IORef PortalStateTimers
    , handlers :: IORef PortalStateHandlers
    } 

data PortalStateHandlers = PSH
    { mouseLeaveHandler :: IO ()
    , mouseEnterHandler :: IO ()
    , clickHandler :: IO ()
    , keydownHandler :: IO ()
    } deriving (Generic,Default)

data PortalStateTimers = PST
    { mouseEnterTimer :: Maybe ThreadId
    , mouseLeaveTimer :: Maybe ThreadId
    } deriving (Generic,Default)

data PortalStateNodes = PSN
    { rootNode :: Maybe JSV
    , portalNode :: Maybe JSV
    , triggerNode :: Maybe JSV
    } deriving (Generic,Default)

instance Typeable ms => Pure Portal ms where
    render p =
        Component "Semantic.Addons.Portal" p $ \self -> 
            let

                handleDocumentClick (parseMaybe (.: "target") -> Just (target :: Obj)) = do
                    PS      {..} <- getState self
                    Portal_ {..} <- getProps self
                    PSN     {..} <- readIORef nodes
                    let check = maybe (return False) (`contains` (unsafeCoerce target))
                    inTrigger <- check triggerNode
                    inPortal  <- check portalNode
                    inRoot    <- check rootNode
                    when ( not ( isNil rootNode 
                                 || isNil portalNode 
                                 || inTrigger 
                                 || inPortal
                               )
                           &&  ( (closeOnDocumentClick && not inRoot)
                                 || (closeOnRootNodeClick && inRoot)
                               )
                         ) closePortal
                handleDocumentClick _ = return ()

                handleEscape (parseMaybe (.: "keyCode") -> Just (27 :: Int)) = do
                    Portal_ {..} <- getProps self
                    when closeOnEscape closePortal
                handleEscape _ = return ()

                handlePortalMouseLeave = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    unless closeOnPortalMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay mouseLeaveDelay
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handlePortalMouseEnter = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    PST {..} <- readIORef timers
                    unless closeOnPortalMouseLeave $
                        for_ mouseLeaveTimer killThread

                handleTriggerBlur (parseMaybe (.: "relatedTarget") -> Just (related :: Obj)) = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    PSN {..} <- readIORef nodes
                    didFocusPortal <- maybe (return False) (`contains` (unsafeCoerce related)) rootNode 
                    unless (closeOnTriggerBlur || not didFocusPortal) 
                        closePortal
                handleTriggerBlur _ = return ()

                handleTriggerClick = do
                    PS {..} <- getState self
                    Portal_ {..} <- getProps self
                    if active && closeOnTriggerClick
                        then closePortal
                        else when (not active && openOnTriggerClick)
                                openPortal

                handleTriggerFocus = do
                    Portal_ {..} <- getProps self
                    when openOnTriggerFocus 
                        openPortal

                handleTriggerMouseLeave = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    when closeOnTriggerMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay mouseLeaveDelay
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handleTriggerMouseEnter = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    when openOnTriggerMouseEnter $ do
                        tid <- forkIO $ do
                            threadDelay mouseEnterDelay
                            openPortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseEnterTimer = Just tid, .. }

                openPortal = do
                    Portal_ {..} <- getProps self
                    setState self $ \_ PS {..} -> PS { active = True, .. }
                    void $ parent self onOpen

                closePortal = do
                    Portal_ {..} <- getProps self
                    setState self $ \_ PS {..} -> PS { active = False, .. }
                    void $ parent self onClose

                renderPortal = do
                    PS {..} <- getState self
                    when active $ do
                        mountPortal
                        Portal_ {..} <- getProps self
                        PS {..} <- getState self
                        PSN {..} <- readIORef nodes
                        PSH {..} <- readIORef handlers
                        for_ rootNode $ \n -> 
                            setProperty (Element n) "className" (Txt.unwords classes)
                        mouseLeaveHandler
                        mouseEnterHandler
                        mtd <- newIORef (return ())
                        new <- Pure.DOM.build (parent self) mtd Nothing (only children)
                        let Just n = getHost new
                        addAnimation $ do
                            for_ rootNode $ \rn -> append (Node rn) n
                            join $ readIORef mtd 
                        pn <- getChild n 0
                        mlh <- maybe (return (return ())) (\n -> onRaw n "mouseleave" def (\_ _ -> handlePortalMouseLeave)) pn
                        meh <- maybe (return (return ())) (\n -> onRaw n "mouseenter" def (\_ _ -> handlePortalMouseEnter)) pn
                        modifyIORef handlers $ \PSH {..} ->
                            PSH { mouseLeaveHandler = mlh
                                , mouseEnterHandler = meh 
                                , .. 
                                }
                        modifyIORef nodes $ \PSN {..} -> 
                            PSN { portalNode = maybe Nothing (\(Node n) -> Just n) pn
                                , .. 
                                }

                mountPortal = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    PSN {..} <- readIORef nodes
                    when (isNil rootNode) $ do
                        let mn = fromMaybe body mountNode
                        Element root <- create "div"
                        if prepend
                            then insertAt (Element mn) (Node root) 0
                            else append   (Node mn) (Node root)
                        Doc d <- getDocument
                        ch <- onRaw (Node d) "click" def (\_ -> handleDocumentClick)
                        kh <- onRaw (Node d) "keydown" def (\_ -> handleEscape)

                        modifyIORef nodes $ \PSN {..} -> 
                            PSN { rootNode = Just root
                                , .. 
                                }
                        modifyIORef handlers $ \PSH {..} -> 
                            PSH { clickHandler = ch
                                , keydownHandler = kh
                                , .. 
                                }

                        parent self onMount

                unmountPortal = do
                    PS {..} <- getState self
                    Portal_ {..} <- getProps self
                    PSN {..} <- readIORef nodes
                    PSH {..} <- readIORef handlers

                    for_ rootNode (removeNode . Node) -- does this clean up well enough?

                    mouseLeaveHandler
                    mouseEnterHandler
                    keydownHandler
                    clickHandler

                    writeIORef handlers def
                    writeIORef nodes def

                    parent self onUnmount
                    

                handleRef (Node r) = do
                    PS {..} <- getState self
                    modifyIORef nodes $ \PSN {..} ->
                        PSN { triggerNode = Just r, .. }

            in
                def
                    { construct = PS (defaultOpen p) 
                                    <$> newIORef def 
                                    <*> newIORef def 
                                    <*> newIORef def
                    , mounted = renderPortal
                    , updated = \_ (active -> wasOpen) _ -> do
                        portal <- getProps self
                        (active -> nowOpen) <- getState self
                        renderPortal
                        when (wasOpen && not nowOpen) unmountPortal
                    , unmount = void $ do
                        PS {..} <- getState self
                        PST {..} <- readIORef timers
                        unmountPortal
                        for mouseEnterTimer killThread
                        for mouseLeaveTimer killThread
                    , renderer = \Portal_ {..} ps -> 
                        trigger #
                            (Proxy $ def & InnerRef handleRef & Children
                                [ cloneWithProps trigger
                                    [ On "blur" def (\e -> handleTriggerBlur (evtObj e) >> return Nothing)
                                    , On "click" def (\_ -> handleTriggerClick >> return Nothing)
                                    , On "focus" def (\_ -> handleTriggerFocus >> return Nothing)
                                    , On "mouseleave" def (\_ -> handleTriggerMouseLeave >> return Nothing)
                                    , On "mouseenter" def (\_ -> handleTriggerMouseEnter >> return Nothing)
                                    ]
                                ]
                            )
                    }

instance HasAttributesProp (Portal ms) where
    type Attribute (Portal ms) = Feature ms
    getAttributes = attributes
    setAttributes as p = p { attributes = as }

instance HasChildrenProp (Portal ms) where
    type Child (Portal ms) = View ms
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasClassesProp (Portal ms) where
    getClasses = classes
    setClasses cs p = p { classes = cs }

instance HasCloseOnDocumentClickProp (Portal ms) where
    getCloseOnDocumentClick = closeOnDocumentClick
    setCloseOnDocumentClick codc p = p { closeOnDocumentClick = codc }

instance HasCloseOnEscapeProp (Portal ms) where
    getCloseOnEscape = closeOnEscape
    setCloseOnEscape coe p = p { closeOnEscape = coe }

instance HasCloseOnPortalMouseLeaveProp (Portal ms) where
    getCloseOnPortalMouseLeave = closeOnPortalMouseLeave
    setCloseOnPortalMouseLeave copml p = p { closeOnPortalMouseLeave = copml }

instance HasCloseOnRootNodeClickProp (Portal ms) where
    getCloseOnRootNodeClick = closeOnRootNodeClick
    setCloseOnRootNodeClick cornc p = p { closeOnRootNodeClick = cornc }

instance HasCloseOnTriggerBlurProp (Portal ms) where
    getCloseOnTriggerBlur = closeOnTriggerBlur
    setCloseOnTriggerBlur cotb p = p { closeOnTriggerBlur = cotb }

instance HasCloseOnTriggerMouseLeaveProp (Portal ms) where
    getCloseOnTriggerMouseLeave = closeOnTriggerMouseLeave
    setCloseOnTriggerMouseLeave cotml p = p { closeOnTriggerMouseLeave = cotml }

instance HasDefaultOpenProp (Portal ms) where
    getDefaultOpen = defaultOpen
    setDefaultOpen o p = p { defaultOpen = o }

instance HasMountNodeProp (Portal ms) where
    getMountNode = mountNode
    setMountNode mn p = p { mountNode = mn }

instance HasMouseEnterDelayProp (Portal ms) where
    getMouseEnterDelay = mouseEnterDelay
    setMouseEnterDelay med p = p { mouseEnterDelay = med }

instance HasMouseLeaveDelayProp (Portal ms) where
    getMouseLeaveDelay = mouseLeaveDelay
    setMouseLeaveDelay mld p = p { mouseLeaveDelay = mld }

instance HasOnCloseProp (Portal ms) where
    type OnCloseProp (Portal ms) = Ef ms IO ()
    getOnClose = onClose
    setOnClose oc p = p { onClose = oc }

instance HasOnMountProp (Portal ms) where
    type OnMountProp (Portal ms) = Ef ms IO ()
    getOnMount = onMount
    setOnMount om p = p { onMount = om }

instance HasOnOpenProp (Portal ms) where
    type OnOpenProp (Portal ms) = Ef ms IO ()
    getOnOpen = onOpen
    setOnOpen oo p = p { onOpen = oo }

instance HasOnUnmountProp (Portal ms) where
    type OnUnmountProp (Portal ms) = Ef ms IO ()
    getOnUnmount = onUnmount
    setOnUnmount ou p = p { onUnmount = ou }

instance HasOpenProp (Portal ms) where
    getOpen = open
    setOpen o p = p { open = o }

instance HasOpenOnTriggerClickProp (Portal ms) where
    getOpenOnTriggerClick = openOnTriggerClick
    setOpenOnTriggerClick ootc p = p { openOnTriggerClick = ootc }

instance HasOpenOnTriggerFocusProp (Portal ms) where
    getOpenOnTriggerFocus = openOnTriggerFocus
    setOpenOnTriggerFocus ootf p = p { openOnTriggerFocus = ootf }

instance HasOpenOnTriggerMouseEnterProp (Portal ms) where
    getOpenOnTriggerMouseEnter = openOnTriggerMouseEnter
    setOpenOnTriggerMouseEnter ootme p = p { openOnTriggerMouseEnter = ootme }

instance HasPrependProp (Portal ms) where
    getPrepend = prepend
    setPrepend pre p = p { prepend = pre }

instance HasTriggerProp (Portal ms) where
    type TriggerProp (Portal ms) = View ms
    getTrigger = trigger
    setTrigger t p = p { trigger = t }