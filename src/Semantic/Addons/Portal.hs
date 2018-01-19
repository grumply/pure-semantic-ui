module Semantic.Addons.Portal where

import Control.Concurrent
import Data.IORef
import Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import GHC.Generics as G
import Pure.Data.Txt as Txt (unwords)
import Pure.View hiding (active,trigger,Proxy)
import Pure.Lifted (getDocument,append,getChild,removeNode,setProperty,create,insertAt,body,IsJSV(..),JSV,Node(..),Element(..),Doc(..),(.#))
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
import Semantic.Properties.CloseOnTriggerClick
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
    , onOpen :: Evt -> Ef ms IO ()
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

pattern Portal :: Portal ms -> View ms
pattern Portal p = View p

data PortalState ms = PS
    { active :: Bool
    , nodes :: IORef PortalStateNodes
    , timers :: IORef PortalStateTimers
    , handlers :: IORef PortalStateHandlers
    , liveView :: IORef (View ms,View ms)
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

instance Pure Portal ms where
    render p =
        Component "Semantic.Addons.Portal" p $ \self -> 
            let

                handleDocumentClick ((.# "target") -> Just (target :: JSV)) = do
                    PS      {..} <- getState self
                    Portal_ {..} <- getProps self
                    PSN     {..} <- readIORef nodes
                    let check = maybe (return False) (`contains` (unsafeCoerce target))
                    inTrigger <- check triggerNode
                    inPortal  <- check portalNode
                    inRoot    <- check rootNode
                    unless ( isNil rootNode || isNil portalNode || inTrigger || inPortal ) $
                        when ( (closeOnDocumentClick && not inRoot) || (closeOnRootNodeClick && inRoot) )
                            closePortal
                handleDocumentClick _ = return ()

                handleEscape ((.# "keyCode") -> Just (27 :: Int)) = do
                    Portal_ {..} <- getProps self
                    when closeOnEscape closePortal
                handleEscape _ = return ()

                handlePortalMouseLeave = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    when closeOnPortalMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay (mouseLeaveDelay * 1000)
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handlePortalMouseEnter = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    PST {..} <- readIORef timers
                    when closeOnPortalMouseLeave $
                        for_ mouseLeaveTimer killThread

                handleTriggerBlur ((.# "relatedTarget") . evtObj -> Just (related :: JSV)) = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    PSN {..} <- readIORef nodes
                    didFocusPortal <- maybe (return False) (`contains` (unsafeCoerce related)) rootNode 
                    unless (not closeOnTriggerBlur || didFocusPortal)
                        closePortal
                handleTriggerBlur _ = return ()

                handleTriggerClick e = do
                    PS {..} <- getState self
                    Portal_ {..} <- getProps self
                    if active && closeOnTriggerClick
                        then closePortal
                        else when (not active && openOnTriggerClick)
                                (openPortal e)

                handleTriggerFocus e = do
                    Portal_ {..} <- getProps self
                    when openOnTriggerFocus 
                        (openPortal e)

                handleTriggerMouseLeave = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    when closeOnTriggerMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay (mouseLeaveDelay * 1000)
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handleTriggerMouseEnter e = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    when openOnTriggerMouseEnter $ do
                        tid <- forkIO $ do
                            threadDelay (mouseEnterDelay * 1000)
                            openPortal e
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseEnterTimer = Just tid, .. }

                openPortal e = do
                    Portal_ {..} <- getProps self
                    setState self $ \_ PS {..} -> PS { active = True, .. }
                    void $ parent self (onOpen e)

                closePortal = do
                    Portal_ {..} <- getProps self
                    setState self $ \_ PS {..} -> PS { active = False, .. }
                    void $ parent self onClose

                renderPortal = do
                    PS {..} <- getState self
                    active # do
                        mountPortal
                        Portal_ {..} <- getProps self
                        PS {..} <- getState self
                        PSN {..} <- readIORef nodes
                        PSH {..} <- readIORef handlers
                        for_ rootNode $ \n -> 
                            setProperty (Element n) "className" (Txt.unwords classes)
                        mouseLeaveHandler
                        mouseEnterHandler
                        let chld = only children
                        mtd <- newIORef (return ())
                        new <- Pure.DOM.build (parent self) mtd Nothing chld
                        writeIORef liveView (chld,new)
                        let Just n = getHost new
                        addAnimation $ do
                            for_ rootNode $ \rn -> append (Node rn) n
                            join $ readIORef mtd 
                        mlh <- onRaw n "mouseleave" def (\_ _ -> handlePortalMouseLeave)
                        meh <- onRaw n "mouseenter" def (\_ _ -> handlePortalMouseEnter)
                        modifyIORef handlers $ \PSH {..} ->
                            PSH { mouseLeaveHandler = mlh
                                , mouseEnterHandler = meh 
                                , .. 
                                }
                        modifyIORef nodes $ \PSN {..} -> 
                            PSN { portalNode = Just $ toJSV n
                                , .. 
                                }

                diffPortal updateRootClasses = do
                    PS {..} <- getState self
                    Portal_ {..} <- getProps self
                    updateRootClasses # do
                        PSN {..} <- readIORef nodes
                        for_ rootNode $ \n -> 
                            setProperty (Element n) "className" (Txt.unwords classes)
                    active # do
                        (mid,old) <- readIORef liveView
                        mtd  <- newIORef (return ())
                        let new = only children
                            (plan,newLive) = buildPlan (\p -> diffDeferred (parent self) mtd p old mid new)
                        m <- plan `seq` readIORef mtd
                        unless (List.null plan) $ do
                            barrier <- newEmptyMVar
                            addAnimation $ runPlan (putMVar barrier ():plan)
                            takeMVar barrier
                        writeIORef liveView (new,newLive)
                        m

                mountPortal = do
                    Portal_ {..} <- getProps self
                    PS {..} <- getState self
                    PSN {..} <- readIORef nodes
                    when (isNil rootNode) $ do
                        let mn = fromMaybe (toJSV body) mountNode
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
                    (_,live) <- readIORef liveView

                    cleanup live
                    for_ rootNode (removeNode . Node)

                    mouseLeaveHandler
                    mouseEnterHandler
                    keydownHandler
                    clickHandler

                    writeIORef handlers def
                    writeIORef nodes PSN { rootNode = def, portalNode = def, .. }

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
                                    <*> newIORef def
                    , mounted = renderPortal
                    , receiveProps = \newprops oldstate -> do
                        oldprops <- getProps self
                        return $
                          (open newprops /= open oldprops)
                            ? oldstate { active = open newprops }
                            $ oldstate
                    , updated = \(classes -> oldCs) (active -> wasOpen@(not -> wasClosed)) _ -> do
                        (classes -> newCs) <- getProps self
                        (active -> nowOpen@(not -> nowClosed)) <- getState self
                        if | wasClosed && nowOpen -> renderPortal
                           | wasOpen && nowClosed -> unmountPortal
                           | nowOpen              -> diffPortal (oldCs /= newCs)
                           | otherwise            -> return ()
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
                                    [ On "blur" def (\e -> handleTriggerBlur e >> return Nothing)
                                    , On "click" def (\e -> handleTriggerClick e >> return Nothing)
                                    , On "focus" def (\e -> handleTriggerFocus e >> return Nothing)
                                    , On "mouseleave" def (\_ -> handleTriggerMouseLeave >> return Nothing)
                                    , On "mouseenter" def (\e -> handleTriggerMouseEnter e >> return Nothing)
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

instance HasCloseOnTriggerClickProp (Portal ms) where
    getCloseOnTriggerClick = closeOnTriggerClick
    setCloseOnTriggerClick cotc p = p { closeOnTriggerClick = cotc }

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
    type OnOpenProp (Portal ms) = Evt -> Ef ms IO ()
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