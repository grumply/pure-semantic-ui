module Semantic.Portal
  ( module Properties
  , module Tools
  , Portal(..), pattern Semantic.Portal.Portal
  ) where

import Pure
import Pure.Data.Txt as Txt (unwords)

import Control.Concurrent
import Control.Monad (when,void)
import Data.Coerce (coerce)
import Data.IORef
import Data.Foldable (for_)
import Data.List as List
import Data.Maybe (isJust,fromMaybe)
import qualified Data.List as List
import Data.Traversable (for)
import GHC.Generics as G

import Semantic.Utils

import Semantic.Proxy

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
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
  , pattern OnUnmounted, OnUnmounted(..)
  , pattern Open, Open(..)
  , pattern OpenOnTriggerClick, OpenOnTriggerClick(..)
  , pattern OpenOnTriggerFocus, OpenOnTriggerFocus(..)
  , pattern OpenOnTriggerMouseEnter, OpenOnTriggerMouseEnter(..)
  , pattern PortalNode, PortalNode(..)
  )

import Data.Function as Tools ((&))

data Portal = Portal_
    { as                       :: Features -> [View] -> View
    , features                 :: Features
    , children                 :: [View]
    , portal                   :: (Features -> Features) -> View
    , mountNode                :: Maybe Element -- ^ Will not work correctly with pure-managed keyed nodes!
    , closeOnDocumentClick     :: Bool
    , closeOnEscape            :: Bool
    , closeOnPortalMouseLeave  :: Bool
    , closeOnRootNodeClick     :: Bool
    , closeOnTriggerBlur       :: Bool
    , closeOnTriggerClick      :: Bool
    , closeOnTriggerMouseLeave :: Bool
    , defaultOpen              :: Bool
    , mouseEnterDelay          :: Int
    , mouseLeaveDelay          :: Int
    , onClose                  :: IO ()
    , onMount                  :: IO ()
    , onOpen                   :: Evt -> IO ()
    , onUnmounted              :: IO ()
    , open                     :: Bool
    , openOnTriggerClick       :: Bool
    , openOnTriggerFocus       :: Bool
    , openOnTriggerMouseEnter  :: Bool
    } deriving (Generic)

instance Default Portal where
    def = (G.to gdef)
            { as                   = \fs cs -> Div & Features fs & Children cs
            , closeOnDocumentClick = True
            , closeOnEscape        = True
            , openOnTriggerClick   = True
            }

pattern Portal :: Portal -> Portal
pattern Portal p = p

data PortalState = PS
    { active   :: Bool
    , nodes    :: IORef PortalStateNodes
    , timers   :: IORef PortalStateTimers
    , liveView :: IORef (View,View)
    }

data PortalStateTimers = PST
    { mouseEnterTimer :: Maybe ThreadId
    , mouseLeaveTimer :: Maybe ThreadId
    } deriving (Generic,Default)

data PortalStateNodes = PSN
    { portalNode  :: Maybe JSV
    , triggerNode :: Maybe JSV
    } deriving (Generic,Default)

instance Pure Portal where
    view =
        LibraryComponentIO $ \self ->
            let

                toRoot = Just . maybe (coerce body) id

                contained Nothing  _ = return False
                contained (Just s) t = toJSV s `contains` toJSV t

                handleDocumentClick (evtTarget -> t) = do
                    PS      {..} <- get self
                    Portal_ {..} <- ask self
                    PSN     {..} <- readIORef nodes

                    inTrigger <- contained triggerNode t
                    inPortal  <- contained portalNode t
                    inRoot    <- contained (toRoot mountNode) t

                    let
                        -- the conditions for checking if the event should close the portal
                        shouldCheck = alive && handleable
                          where
                            alive      = isJust portalNode
                            handleable = not inTrigger && not inPortal

                        -- the two cases in which the portal should close
                        shouldClose = cond1 || cond2
                          where
                            cond1 = closeOnDocumentClick && not inRoot
                            cond2 = closeOnRootNodeClick &&     inRoot

                        eventShouldClosePortal = shouldCheck && shouldClose

                    when eventShouldClosePortal closePortal

                handleEscape Escape = do
                    Portal_ {..} <- ask self
                    when closeOnEscape closePortal
                handleEscape _ = return ()

                handlePortalMouseLeave _ = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    when closeOnPortalMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay (mouseLeaveDelay * 1000)
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handlePortalMouseEnter _ = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    PST {..} <- readIORef timers
                    when closeOnPortalMouseLeave $
                        for_ mouseLeaveTimer killThread

                handleTriggerBlur (RelatedTarget r) = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    PSN {..} <- readIORef nodes
                    didFocusPortal <- contained (toRoot mountNode) r
                    when (closeOnTriggerBlur && not didFocusPortal)
                        closePortal
                handleTriggerBlur _ = return ()

                handleTriggerClick e = do
                    PS {..} <- get self
                    Portal_ {..} <- ask self
                    if active && closeOnTriggerClick
                        then closePortal
                        else when (not active && openOnTriggerClick)
                                (openPortal e)

                handleTriggerFocus e = do
                    Portal_ {..} <- ask self
                    when openOnTriggerFocus
                        (openPortal e)

                handleTriggerMouseLeave _ = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    when closeOnTriggerMouseLeave $ do
                        tid <- forkIO $ do
                            threadDelay (mouseLeaveDelay * 1000)
                            closePortal
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseLeaveTimer = Just tid, .. }

                handleTriggerMouseEnter e = do
                    Portal_ {..} <- ask self
                    PS {..} <- get self
                    when openOnTriggerMouseEnter $ do
                        tid <- forkIO $ do
                            threadDelay (mouseEnterDelay * 1000)
                            openPortal e
                        modifyIORef timers $ \PST {..} ->
                            PST { mouseEnterTimer = Just tid, .. }

                openPortal e = do
                    Portal_ {..} <- ask self
                    modify self $ \_ PS {..} -> PS { active = True, .. }
                    onOpen e

                closePortal = do
                    Portal_ {..} <- ask self
                    modify self $ \_ PS {..} -> PS { active = False, .. }
                    onClose

                handleTriggerRef (Node r) = do
                    PS {..} <- get self
                    modifyIORef nodes $ \PSN {..} ->
                        PSN { triggerNode = Just r, .. }

                handlePortalRef (Node r) = do
                    PS {..} <- get self
                    modifyIORef nodes $ \PSN {..} ->
                        PSN { portalNode = Just r, .. }

            in
                def
                    { construct = do
                        Portal_ {..} <- ask self
                        PS defaultOpen
                          <$> newIORef def
                          <*> newIORef def
                          <*> newIORef def

                    , receive = \newprops oldstate -> do
                        oldprops <- ask self
                        let change = open newprops /= open oldprops
                        if | change && open newprops -> do
                            Semantic.Portal.onMount newprops
                            return oldstate { active = open newprops }
                           | change -> do
                            Semantic.Portal.onUnmounted newprops
                            return oldstate { active = open newprops }
                           | otherwise -> return oldstate

                    , unmounted = void $ do
                        PS  {..} <- get self
                        PST {..} <- readIORef timers
                        for mouseEnterTimer killThread
                        for mouseLeaveTimer killThread

                    , render = \Portal_ {..} ps ->
                        let
                            p | active ps = PortalView Nothing (fromMaybe (coerce body) mountNode) $ portal
                                          $ WithHost handlePortalRef
                                          . OnMouseEnter handlePortalMouseEnter
                                          . OnMouseLeave handlePortalMouseLeave
                                          . OnDoc "click" handleDocumentClick
                                          . OnDoc "keydown" handleEscape
                              | otherwise = Null

                            addTriggerHandlers =
                                WithHost handleTriggerRef
                              . OnBlur handleTriggerBlur
                              . OnClick handleTriggerClick
                              . OnFocus handleTriggerFocus
                              . OnMouseLeave handleTriggerMouseLeave
                              . OnMouseEnter handleTriggerMouseEnter

                        in
                            as
                                (features & addTriggerHandlers)
                                (p : children)
                    }

instance HasProp As Portal where
    type Prop As Portal = Features -> [View] -> View
    getProp _ = as
    setProp _ a p = p { as = a }

instance HasFeatures Portal where
    getFeatures = features
    setFeatures as p = p { features = as }

instance HasChildren Portal where
    getChildren = children
    setChildren cs p = p { children = cs }

instance HasProp PortalNode Portal where
    type Prop PortalNode Portal = (Features -> Features) -> View
    getProp _ = portal
    setProp _ pn p = p { portal = pn }

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
    type Prop MountNode Portal = Maybe Element
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

instance HasProp OnUnmounted Portal where
    type Prop OnUnmounted Portal = IO ()
    getProp _ = onUnmounted
    setProp _ ou p = p { onUnmounted = ou }

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
