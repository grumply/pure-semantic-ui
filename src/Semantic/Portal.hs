module Semantic.Portal
  ( module Properties
  , module Tools
  , Portal(..), pattern Portal
  ) where

import Control.Concurrent
import Data.IORef
import Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML
import Pure.Data.Event
import Pure.Data.Txt as Txt (unwords)
import Pure.Lifted (getDocument,append,getChild,removeNode,setProperty,create,insertAt,body,IsJSV(..),JSV,Node(..),Element(..),Doc(..),(.#))
import Pure.DOM (onRaw)

import Semantic.Utils

import Semantic.Proxy

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern CloseOnDocumentClick, CloseOnDocumentClick(..)
  , pattern CloseOnEscape, CloseOnEscape(..)
  , pattern CloseOnPortalMouseLeave, CloseOnPortalMouseLeave(..)
  , pattern CloseOnRootNodeClick, CloseOnRootNodeClick(..)
  , pattern CloseOnTriggerBlur, CloseOnTriggerBlur(..)
  , pattern CloseOnTriggerClick, CloseOnTriggerClick(..)
  , pattern CloseOnTriggerMouseLeave, CloseOnTriggerMouseLeave(..)
  , pattern DefaultOpen, DefaultOpen(..)
  , pattern MountNode, MountNode(..)
  , pattern MouseEnterDelay, MouseEnterDelay(..)
  , pattern MouseLeaveDelay, MouseLeaveDelay(..)
  , pattern OnClose, OnClose(..)
  , pattern OnMount, OnMount(..)
  , pattern OnOpen, OnOpen(..)
  , pattern OnUnmount, OnUnmount(..)
  , pattern Open, Open(..)
  , pattern OpenOnTriggerClick, OpenOnTriggerClick(..)
  , pattern OpenOnTriggerFocus, OpenOnTriggerFocus(..)
  , pattern OpenOnTriggerMouseEnter, OpenOnTriggerMouseEnter(..)
  , pattern Prepend, Prepend(..)
  , pattern Trigger, Trigger(..)
  , pattern InnerRef, InnerRef(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

-- used safely
import Unsafe.Coerce

data Portal = Portal_
    { attributes               :: Features
    , children                 :: [View]
    , closeOnDocumentClick     :: Bool
    , closeOnEscape            :: Bool
    , closeOnPortalMouseLeave  :: Bool
    , closeOnRootNodeClick     :: Bool
    , closeOnTriggerBlur       :: Bool
    , closeOnTriggerClick      :: Bool
    , closeOnTriggerMouseLeave :: Bool
    , defaultOpen              :: Bool
    , mountNode                :: Maybe JSV
    , mouseEnterDelay          :: Int
    , mouseLeaveDelay          :: Int
    , onClose                  :: IO ()
    , onMount                  :: IO ()
    , onOpen                   :: Evt -> IO ()
    , onUnmount                :: IO ()
    , open                     :: Bool
    , openOnTriggerClick       :: Bool
    , openOnTriggerFocus       :: Bool
    , openOnTriggerMouseEnter  :: Bool
    , prepend                  :: Bool
    , trigger                  :: View
    } deriving (Generic)

instance Default Portal where
    def = (G.to gdef)
            { closeOnDocumentClick = True
            , closeOnEscape        = True
            , openOnTriggerClick   = True
            }

pattern Portal :: Portal -> Portal
pattern Portal p = p

data PortalState = PS
    { active   :: Bool
    , nodes    :: IORef PortalStateNodes
    , timers   :: IORef PortalStateTimers
    , handlers :: IORef PortalStateHandlers
    , liveView :: IORef (View,View)
    }

data PortalStateHandlers = PSH
    { mouseLeaveHandler :: IO ()
    , mouseEnterHandler :: IO ()
    , clickHandler      :: IO ()
    , keydownHandler    :: IO ()
    } deriving (Generic,Default)

data PortalStateTimers = PST
    { mouseEnterTimer :: Maybe ThreadId
    , mouseLeaveTimer :: Maybe ThreadId
    } deriving (Generic,Default)

data PortalStateNodes = PSN
    { rootNode    :: Maybe JSV
    , portalNode  :: Maybe JSV
    , triggerNode :: Maybe JSV
    } deriving (Generic,Default)

instance Pure Portal where
    view =
        LibraryComponentIO $ \self ->
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
                    onOpen e

                closePortal = do
                    Portal_ {..} <- getProps self
                    setState self $ \_ PS {..} -> PS { active = False, .. }
                    onClose

                viewPortal = do
                    PS {..} <- getState self
                    active # do
                        mountPortal
                        Portal_ {..} <- getProps self
                        PS {..} <- getState self
                        PSN {..} <- readIORef nodes
                        PSH {..} <- readIORef handlers
                        for_ rootNode $ \n ->
                        mouseLeaveHandler
                        mouseEnterHandler
                        let chld = only children
                        mtd <- newIORef (return ())
                        new <- Pure.DOM.build mtd Nothing chld
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

                    PS {..} <- getState self
                    Portal_ {..} <- getProps self
                        PSN {..} <- readIORef nodes
                        for_ rootNode $ \n ->
                    active # do
                        (mid,old) <- readIORef liveView
                        mtd  <- newIORef (return ())
                        let new = only children
                            (plan,newLive) = buildPlan (\p -> diffDeferred mtd p old mid new)
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

                        onMount

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

                    onUnmount

                    writeIORef liveView (nil,nil)

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
                    , mounted = viewPortal
                    , receiveProps = \newprops oldstate -> do
                        oldprops <- getProps self
                        return $
                          (open newprops /= open oldprops)
                            ? oldstate { active = open newprops }
                            $ oldstate
                        (active -> nowOpen@(not -> nowClosed)) <- getState self
                        if | wasClosed && nowOpen -> viewPortal
                           | wasOpen && nowClosed -> unmountPortal
                           | nowOpen              -> diffPortal (oldCs /= newCs)
                           | otherwise            -> return ()
                    , unmount = void $ do
                        PS {..} <- getState self
                        PST {..} <- readIORef timers
                        unmountPortal
                        for mouseEnterTimer killThread
                        for mouseLeaveTimer killThread
                    , render = \Portal_ {..} ps ->
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

instance HasFeatures Portal where
    getFeatures = features
    setFeatures as p = p { features = as }

instance HasChildren Portal where
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasProp CloseOnDocumentClick Portal where
    type Prop CloseOnDocumentClick Portal = Bool
    getProp _ = closeOnDocumentClick
    setProp _ codc p = p { closeOnDocumentClick = codc }

instance HasProp CloseOnEscape Portal where
    type Prop CloseOnEscape Portal = Bool
    getProp _ = closeOnEscape
    setProp _ coe p = p { closeOnEscape = coe }

instance HasProp CloseOnPortalMouseLeave Portal where
    type Prop CloseOnPortalMouseLeave Portal = Bool
    getProp _ = closeOnPortalMouseLeave
    setProp _ copml p = p { closeOnPortalMouseLeave = copml }

instance HasProp CloseOnRootNodeClick Portal where
    type Prop CloseOnRootNodeClick Portal = Bool
    getProp _ = closeOnRootNodeClick
    setProp _ cornc p = p { closeOnRootNodeClick = cornc }

instance HasProp CloseOnTriggerBlur Portal where
    type Prop CloseOnTriggerBlur Portal = Bool
    getProp _ = closeOnTriggerBlur
    setProp _ cotb p = p { closeOnTriggerBlur = cotb }

instance HasProp CloseOnTriggerClick Portal where
    type Prop CloseOnTriggerClick Portal = Bool
    getProp _ = closeOnTriggerClick
    setProp _ cotc p = p { closeOnTriggerClick = cotc }

instance HasProp CloseOnTriggerMouseLeave Portal where
    type Prop CloseOnTriggerMouseLeave Portal = Bool
    getProp _ = closeOnTriggerMouseLeave
    setProp _ cotml p = p { closeOnTriggerMouseLeave = cotml }

instance HasProp DefaultOpen Portal where
    type Prop DefaultOpen Portal = Bool
    getProp _ = defaultOpen
    setProp _ o p = p { defaultOpen = o }

instance HasProp MountNode Portal where
    type Prop MountNode Portal = Maybe JSV
    getProp _ = mountNode
    setProp _ mn p = p { mountNode = mn }

instance HasProp MouseEnterDelay Portal where
    type Prop MouseEnterDelay Portal = Int
    getProp _ = mouseEnterDelay
    setProp _ med p = p { mouseEnterDelay = med }

instance HasProp MouseLeaveDelay Portal where
    type Prop MouseLeaveDelay Portal = Int
    getProp _ = mouseLeaveDelay
    setProp _ mld p = p { mouseLeaveDelay = mld }

instance HasProp OnClose Portal where
    type Prop OnClose Portal = IO ()
    getProp _ = onClose
    setProp _ oc p = p { onClose = oc }

instance HasProp OnMount Portal where
    type Prop OnMount Portal = IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnOpen Portal where
    type Prop OnOpen Portal = Evt -> IO ()
    getProp _ = onOpen
    setProp _ oo p = p { onOpen = oo }

instance HasProp OnUnmount Portal where
    type Prop OnUnmount Portal = IO ()
    getProp _ = onUnmount
    setProp _ ou p = p { onUnmount = ou }

instance HasProp Open Portal where
    type Prop Open Portal = Bool
    getProp _ = open
    setProp _ o p = p { open = o }

instance HasProp OpenOnTriggerClick Portal where
    type Prop OpenOnTriggerClick Portal = Bool
    getProp _ = openOnTriggerClick
    setProp _ ootc p = p { openOnTriggerClick = ootc }

instance HasProp OpenOnTriggerFocus Portal where
    type Prop OpenOnTriggerFocus Portal = Bool
    getProp _ = openOnTriggerFocus
    setProp _ ootf p = p { openOnTriggerFocus = ootf }

instance HasProp OpenOnTriggerMouseEnter Portal where
    type Prop OpenOnTriggerMouseEnter Portal = Bool
    getProp _ = openOnTriggerMouseEnter
    setProp _ ootme p = p { openOnTriggerMouseEnter = ootme }

instance HasProp Prepend Portal where
    type Prop Prepend Portal = Bool
    getProp _ = prepend
    setProp _ pre p = p { prepend = pre }

instance HasProp Trigger Portal where
    type Prop Trigger Portal = View
    getProp _ = trigger
    setProp _ t p = p { trigger = t }
