{-# LANGUAGE UndecidableInstances #-}
module Semantic.Sticky
  ( module Properties
  , module Tools
  , Sticky(..), pattern Sticky
  ) where

import Pure hiding (offset,context,not,active,(#))

import Control.Monad
import Data.IORef
import Data.Maybe
import GHC.Generics as G

import Semantic.Utils hiding (body)

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Active, Active(..)
  , pattern BottomOffset, BottomOffset(..)
  , pattern Context, Context(..)
  , pattern Offset, Offset(..)
  , pattern OnBottom, OnBottom(..)
  , pattern OnStick, OnStick(..)
  , pattern OnTop, OnTop(..)
  , pattern OnUnstick, OnUnstick(..)
  , pattern Pushing, Pushing(..)
  , pattern ScrollContext, ScrollContext(..)
  )

import Data.Function as Tools ((&))

data Sticky = Sticky_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , bottomOffset :: Double
    , context :: Maybe JSV
    , offset :: Double
    , onBottom :: IO ()
    , onStick :: IO ()
    , onTop :: IO ()
    , onUnstick :: IO ()
    , pushing :: Bool
    , scrollContext :: Maybe JSV
    } deriving (Generic)

instance Default Sticky where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
        , active = True
        , bottomOffset = 0
        , context = Just (toJSV body)
        , scrollContext = Just (toJSV window)
        }

pattern Sticky :: Sticky -> Sticky
pattern Sticky s = s

data StickyState = SS
    { isSticking :: Bool
    , bottom :: Maybe Double
    , top :: Maybe Double
    , isPushing :: Bool
    , triggerWidth :: Double
    , triggerRef :: IORef (Maybe JSV)
    , stickyRef :: IORef (Maybe JSV)
    , ticking :: IORef Bool
    , resizeListener :: IORef (IO ())
    , scrollListener :: IORef (IO ())
    }

instance Pure Sticky where
    view =
        Component $ \self ->
            let
                getRects = do
                    Sticky_ {..} <- ask self
                    SS {..} <- get self

                    mtr <- readIORef triggerRef
                    tr <- case mtr of
                        Just tr -> boundingRect (Element tr)
                        Nothing -> return def

                    cr <- boundingRect (Element $ fromMaybe (toJSV body) context)

                    r <- readIORef stickyRef
                    sr <- case r of
                        Just sr -> boundingRect (Element sr)
                        Nothing -> return def

                    return (tr,cr,sr)

                upd (triggerRect,contextRect,stickyRect) = do
                    s <- ask self
                    ss@SS {..} <- get self
                    writeIORef ticking False
                    ih <- innerHeight
                    (brWidth triggerRect /= triggerWidth) #
                        modify_ self (\_ SS {..} -> SS { triggerWidth = brWidth triggerRect, .. })
                    void (upd' s ss ih)
                    where
                        upd' Sticky_ {..} SS {..} (fromIntegral -> ih)
                            | isPushing && brTop stickyRect <= brTop triggerRect    = stickToContextTop
                            | isPushing && brBottom contextRect + bottomOffset > ih = stickToScreenBottom
                            | isPushing                                             = stickToContextBottom
                            | brHeight stickyRect > ih && brTop contextRect > 0     = stickToContextTop
                            | brHeight stickyRect > ih && brBottom contextRect < ih = stickToContextBottom
                            | brTop triggerRect < offset &&
                              brHeight stickyRect + offset >= brBottom contextRect  = stickToContextBottom
                            | brTop triggerRect < offset                            = stickToScreenTop
                            | True                                                  = stickToContextTop
                            where

                                setPushing p = pushing #
                                    modify_ self (\_ SS {..} -> SS { isPushing = p, .. })

                                setSticking sticking = void $ do
                                    modify_ self $ \_ SS {..} -> SS { isSticking = sticking, .. }
                                    sticking
                                        ? onStick
                                        $ onUnstick

                                stickToContextBottom = void $ do
                                    onBottom
                                    setSticking True
                                    modify self $ \_ SS {..} ->
                                        SS { top = Just (brBottom contextRect - brHeight stickyRect)
                                           , bottom = Nothing
                                           , ..
                                           }
                                    setPushing True

                                stickToContextTop = void $ do
                                    onTop
                                    setSticking False
                                    setPushing False

                                stickToScreenBottom = do
                                    setSticking True
                                    modify_ self $ \_ SS {..} ->
                                        SS { bottom = Just bottomOffset
                                           , top = Nothing
                                           , ..
                                           }

                                stickToScreenTop = do
                                    setSticking True
                                    modify_ self $ \_ SS {..} ->
                                        SS { top = Just offset
                                           , bottom = Nothing
                                           , ..
                                           }

                handleUpdate = do
                    SS {..} <- get self
                    tckng <- readIORef ticking
                    (not tckng) # do
                        writeIORef ticking True
                        void $ addAnimation $ do
                            newrects <- getRects
                            upd newrects

                addListeners Sticky_ {..} = do
                    SS {..} <- get self
                    let sc = fromMaybe (toJSV window) scrollContext
                    sl <- onRaw (Node sc) "scroll" def (\_ _ -> handleUpdate)
                    writeIORef scrollListener sl
                    rl <- onRaw (Node sc) "resize" def (\_ _ -> handleUpdate)
                    writeIORef resizeListener rl

                removeListeners = do
                    SS {..} <- get self
                    join $ readIORef resizeListener
                    join $ readIORef scrollListener

                handleStickyRef (Node n) = do
                    SS {..} <- get self
                    writeIORef stickyRef (Just n)

                handleTriggerRef (Node n) = do
                    SS {..} <- get self
                    writeIORef triggerRef (Just n)

            in def
                { construct =
                    SS def def def def def
                        <$> newIORef def
                        <*> newIORef def
                        <*> newIORef def
                        <*> newIORef def
                        <*> newIORef def

                , mounted = do
                    s@Sticky_ {..} <- ask self
                    active # do
                        handleUpdate
                        addListeners s

                , receive = \newprops oldstate -> do
                    oldprops <- ask self
                    let newContext =
                          case (scrollContext oldprops,scrollContext newprops) of
                            (Just x,Just y) -> same x y
                            (Nothing,Nothing) -> False
                            _ -> True
                    if | active newprops == active oldprops -> do
                         when newContext $ do
                           removeListeners
                           addListeners newprops
                         return oldstate
                       | active newprops -> handleUpdate >> addListeners newprops >> return oldstate
                       | True -> removeListeners >> return oldstate { isSticking = False }

                , unmounted = do
                    Sticky_ {..} <- ask self
                    removeListeners

                , render = \Sticky_ {..} SS {..} ->
                    let
                        computedStyles = isSticking #
                            [ maybe def (\b -> ("bottom",b <#> px)) bottom
                            , maybe def (\t -> ("top",t <#> px)) top
                            , (position,fixed)
                            , (triggerWidth /= 0) # (width,triggerWidth <#> px)
                            ]
                    in
                        as features
                            [ Div <| WithHost handleTriggerRef
                            , Div <| WithHost handleStickyRef . Styles computedStyles |> children
                            ]

                }

instance HasProp As Sticky where
    type Prop As Sticky = Features -> [View] -> View
    getProp _ = as
    setProp _ f s = s { as = f }

instance HasFeatures Sticky where
    getFeatures = features
    setFeatures cs s = s { features = cs }

instance HasChildren Sticky where
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasProp Active Sticky where
    type Prop Active Sticky = Bool
    getProp _ = active
    setProp _ a s = s { active = a }

instance HasProp BottomOffset Sticky where
    type Prop BottomOffset Sticky = Double
    getProp _ = bottomOffset
    setProp _ bo s = s { bottomOffset = bo }

instance HasProp Context Sticky where
    type Prop Context Sticky = Maybe JSV
    getProp _ = context
    setProp _ c s = s { context = c }

instance HasProp Offset Sticky where
    type Prop Offset Sticky = Double
    getProp _ = offset
    setProp _ o s = s { offset = o }

instance HasProp OnBottom Sticky where
    type Prop OnBottom Sticky = IO ()
    getProp _ = onBottom
    setProp _ ob s = s { onBottom = ob }

instance HasProp OnStick Sticky where
    type Prop OnStick Sticky = IO ()
    getProp _ = onStick
    setProp _ os s = s { onStick = os }

instance HasProp OnTop Sticky where
    type Prop OnTop Sticky = IO ()
    getProp _ = onTop
    setProp _ ot s = s { onTop = ot }

instance HasProp OnUnstick Sticky where
    type Prop OnUnstick Sticky = IO ()
    getProp _ = onUnstick
    setProp _ ou s = s { onUnstick = ou }

instance HasProp Pushing Sticky where
    type Prop Pushing Sticky = Bool
    getProp _ = pushing
    setProp _ p s = s { pushing = p }

instance HasProp ScrollContext Sticky where
    type Prop ScrollContext Sticky = Maybe JSV
    getProp _ = scrollContext
    setProp _ sc s = s { scrollContext = sc }
