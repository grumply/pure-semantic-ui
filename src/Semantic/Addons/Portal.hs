module Semantic.Addons.Portal
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
import Pure.Data.Txt as Txt (unwords)
import Pure.View hiding (active,trigger,Proxy)
import Pure.Lifted (getDocument,append,getChild,removeNode,setProperty,create,insertAt,body,IsJSV(..),JSV,Node(..),Element(..),Doc(..),(.#))
import Pure.DOM

import Semantic.Utils

import Semantic.Addons.Proxy

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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

-- used safely
import Unsafe.Coerce

data Portal ms = Portal_
    { attributes               :: [Feature ms]
    , children                 :: [View ms]
    , classes                  :: [Txt]
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
    , onClose                  :: Ef ms IO ()
    , onMount                  :: Ef ms IO ()
    , onOpen                   :: Evt -> Ef ms IO ()
    , onUnmount                :: Ef ms IO ()
    , open                     :: Bool
    , openOnTriggerClick       :: Bool
    , openOnTriggerFocus       :: Bool
    , openOnTriggerMouseEnter  :: Bool
    , prepend                  :: Bool
    , trigger                  :: View ms
    } deriving (Generic)

instance Default (Portal ms) where
    def = (G.to gdef)
            { closeOnDocumentClick = True
            , closeOnEscape        = True
            , openOnTriggerClick   = True
            }

pattern Portal :: Portal ms -> View ms
pattern Portal p = View p

data PortalState ms = PS
    { active   :: Bool
    , nodes    :: IORef PortalStateNodes
    , timers   :: IORef PortalStateTimers
    , handlers :: IORef PortalStateHandlers
    , liveView :: IORef (View ms,View ms)
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

instance HasProp Attributes (Portal ms) where
    type Prop Attributes (Portal ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as p = p { attributes = as }

instance HasProp Children (Portal ms) where
    type Prop Children (Portal ms) = [View ms]
    getProp _ = children
    setProp _ cs p = p { children = cs }

instance HasProp Classes (Portal ms) where
    type Prop Classes (Portal ms) = [Txt]
    getProp _ = classes
    setProp _ cs p = p { classes = cs }

instance HasProp CloseOnDocumentClick (Portal ms) where
    type Prop CloseOnDocumentClick (Portal ms) = Bool
    getProp _ = closeOnDocumentClick
    setProp _ codc p = p { closeOnDocumentClick = codc }

instance HasProp CloseOnEscape (Portal ms) where
    type Prop CloseOnEscape (Portal ms) = Bool
    getProp _ = closeOnEscape
    setProp _ coe p = p { closeOnEscape = coe }

instance HasProp CloseOnPortalMouseLeave (Portal ms) where
    type Prop CloseOnPortalMouseLeave (Portal ms) = Bool
    getProp _ = closeOnPortalMouseLeave
    setProp _ copml p = p { closeOnPortalMouseLeave = copml }

instance HasProp CloseOnRootNodeClick (Portal ms) where
    type Prop CloseOnRootNodeClick (Portal ms) = Bool
    getProp _ = closeOnRootNodeClick
    setProp _ cornc p = p { closeOnRootNodeClick = cornc }

instance HasProp CloseOnTriggerBlur (Portal ms) where
    type Prop CloseOnTriggerBlur (Portal ms) = Bool
    getProp _ = closeOnTriggerBlur
    setProp _ cotb p = p { closeOnTriggerBlur = cotb }

instance HasProp CloseOnTriggerClick (Portal ms) where
    type Prop CloseOnTriggerClick (Portal ms) = Bool
    getProp _ = closeOnTriggerClick
    setProp _ cotc p = p { closeOnTriggerClick = cotc }

instance HasProp CloseOnTriggerMouseLeave (Portal ms) where
    type Prop CloseOnTriggerMouseLeave (Portal ms) = Bool
    getProp _ = closeOnTriggerMouseLeave
    setProp _ cotml p = p { closeOnTriggerMouseLeave = cotml }

instance HasProp DefaultOpen (Portal ms) where
    type Prop DefaultOpen (Portal ms) = Bool
    getProp _ = defaultOpen
    setProp _ o p = p { defaultOpen = o }

instance HasProp MountNode (Portal ms) where
    type Prop MountNode (Portal ms) = Maybe JSV
    getProp _ = mountNode
    setProp _ mn p = p { mountNode = mn }

instance HasProp MouseEnterDelay (Portal ms) where
    type Prop MouseEnterDelay (Portal ms) = Int
    getProp _ = mouseEnterDelay
    setProp _ med p = p { mouseEnterDelay = med }

instance HasProp MouseLeaveDelay (Portal ms) where
    type Prop MouseLeaveDelay (Portal ms) = Int
    getProp _ = mouseLeaveDelay
    setProp _ mld p = p { mouseLeaveDelay = mld }

instance HasProp OnClose (Portal ms) where
    type Prop OnClose (Portal ms) = Ef ms IO ()
    getProp _ = onClose
    setProp _ oc p = p { onClose = oc }

instance HasProp OnMount (Portal ms) where
    type Prop OnMount (Portal ms) = Ef ms IO ()
    getProp _ = onMount
    setProp _ om p = p { onMount = om }

instance HasProp OnOpen (Portal ms) where
    type Prop OnOpen (Portal ms) = Evt -> Ef ms IO ()
    getProp _ = onOpen
    setProp _ oo p = p { onOpen = oo }

instance HasProp OnUnmount (Portal ms) where
    type Prop OnUnmount (Portal ms) = Ef ms IO ()
    getProp _ = onUnmount
    setProp _ ou p = p { onUnmount = ou }

instance HasProp Open (Portal ms) where
    type Prop Open (Portal ms) = Bool
    getProp _ = open
    setProp _ o p = p { open = o }

instance HasProp OpenOnTriggerClick (Portal ms) where
    type Prop OpenOnTriggerClick (Portal ms) = Bool
    getProp _ = openOnTriggerClick
    setProp _ ootc p = p { openOnTriggerClick = ootc }

instance HasProp OpenOnTriggerFocus (Portal ms) where
    type Prop OpenOnTriggerFocus (Portal ms) = Bool
    getProp _ = openOnTriggerFocus
    setProp _ ootf p = p { openOnTriggerFocus = ootf }

instance HasProp OpenOnTriggerMouseEnter (Portal ms) where
    type Prop OpenOnTriggerMouseEnter (Portal ms) = Bool
    getProp _ = openOnTriggerMouseEnter
    setProp _ ootme p = p { openOnTriggerMouseEnter = ootme }

instance HasProp Prepend (Portal ms) where
    type Prop Prepend (Portal ms) = Bool
    getProp _ = prepend
    setProp _ pre p = p { prepend = pre }

instance HasProp Trigger (Portal ms) where
    type Prop Trigger (Portal ms) = View ms
    getProp _ = trigger
    setProp _ t p = p { trigger = t }
