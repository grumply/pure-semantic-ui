{-# LANGUAGE UndecidableInstances #-}
module Semantic.Sticky
  ( module Properties
  , module Tools
  , Sticky(..), pattern Sticky
  ) where

import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.Lifted (same,window,body,IsJSV(..),JSV,Node(..),Element(..))
import Pure.View hiding (active,bottom,offset,top,round,Offset)
import Pure.DOM (addAnimation)

import Semantic.Utils hiding (body)

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
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
import Pure.Data.Default as Tools

data Sticky ms = Sticky_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , bottomOffset :: Double
    , context :: Maybe JSV
    , offset :: Double
    , onBottom :: Ef ms IO ()
    , onStick :: Ef ms IO ()
    , onTop :: Ef ms IO ()
    , onUnstick :: Ef ms IO ()
    , pushing :: Bool
    , scrollContext :: Maybe JSV
    } deriving (Generic)

instance Default (Sticky ms) where
    def = (G.to gdef)
        { as = Div
        , active = True
        , bottomOffset = 0
        , context = Just (toJSV body)
        , scrollContext = Just (toJSV window)
        }

pattern Sticky :: VC ms => Sticky ms -> View ms
pattern Sticky s = View s

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

instance VC ms => Pure Sticky ms where
    render s =
        Component "Semantic.Modules.Sticky" s $ \self ->
            let
                getRects = do
                    Sticky_ {..} <- getProps self
                    SS {..} <- getState self

                    mtr <- readIORef triggerRef
                    tr <- case mtr of
                        Just tr -> boundingRect (Element tr)
                        Nothing -> return def

                    cr <- boundingRect (Element $ fromMaybe (toJSV body) context)

                    msr <- readIORef stickyRef
                    sr <- case msr of
                        Just sr -> boundingRect (Element sr)
                        Nothing -> return def

                    return (tr,cr,sr)

                upd (triggerRect,contextRect,stickyRect) = do
                    s <- getProps self
                    ss@SS {..} <- getState self
                    writeIORef ticking False
                    ih <- innerHeight
                    (brWidth triggerRect /= triggerWidth) #
                        void (setState self $ \_ SS {..} -> SS { triggerWidth = brWidth triggerRect, .. })
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

                                setPushing p = pushing # do
                                    void (setState self $ \_ SS {..} -> SS { isPushing = p, .. })

                                setSticking sticking = void $ do
                                    setState self $ \_ SS {..} -> SS { isSticking = sticking, .. }
                                    sticking
                                        ? (onStick   # parent self onStick)
                                        $ (onUnstick # parent self onUnstick)

                                stickToContextBottom = void $ do
                                    onBottom # parent self onBottom
                                    setSticking True
                                    setState self $ \_ SS {..} -> SS
                                        { top = Just (brBottom contextRect - brHeight stickyRect)
                                        , bottom = Nothing
                                        , ..
                                        }
                                    setPushing True

                                stickToContextTop = void $ do
                                    onTop # parent self onTop
                                    setSticking False
                                    setPushing False

                                stickToScreenBottom = void $ do
                                    setSticking True
                                    setState self $ \_ SS {..} -> SS
                                        { bottom = Just bottomOffset
                                        , top = Nothing
                                        , ..
                                        }

                                stickToScreenTop = void $ do
                                    setSticking True
                                    setState self $ \_ SS {..} -> SS
                                        { top = Just offset
                                        , bottom = Nothing
                                        , ..
                                        }

                handleUpdate = do
                    SS {..} <- getState self
                    tckng <- readIORef ticking
                    (not tckng) # do
                        writeIORef ticking True
                        void $ addAnimation $ do
                            newrects <- getRects
                            upd newrects

                addListeners Sticky_ {..} = do
                    SS {..} <- getState self
                    let sc = fromMaybe (toJSV window) scrollContext
                    sl <- onRaw (Node sc) "scroll" def (\_ _ -> handleUpdate)
                    writeIORef scrollListener sl
                    rl <- onRaw (Node sc) "resize" def (\_ _ -> handleUpdate)
                    writeIORef resizeListener rl

                removeListeners = do
                    SS {..} <- getState self
                    join $ readIORef resizeListener
                    join $ readIORef scrollListener

                handleStickyRef (Node n) = do
                    SS {..} <- getState self
                    writeIORef stickyRef (Just n)
                    return Nothing

                handleTriggerRef (Node n) = do
                    SS {..} <- getState self
                    writeIORef triggerRef (Just n)
                    return Nothing

            in def
                { construct =
                    SS def def def def def
                        <$> newIORef def
                        <*> newIORef def
                        <*> newIORef def
                        <*> newIORef def
                        <*> newIORef def

                , mounted = do
                    s@Sticky_ {..} <- getProps self
                    active # do
                        handleUpdate
                        addListeners s

                , receiveProps = \newprops oldstate -> do
                    oldprops <- getProps self
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

                , unmount = do
                    Sticky_ {..} <- getProps self
                    removeListeners

                , renderer = \Sticky_ {..} SS {..} ->
                    let
                        computedStyles = isSticking #
                            [ maybe def (\b -> ("bottom",pxs $ round b)) bottom
                            , maybe def (\t -> ("top",pxs $ round t)) top
                            , ("position","fixed")
                            , triggerWidth # ("width",pxs $ round triggerWidth)
                            ]
                    in
                        as
                            ( mergeClasses $ ClassList classes
                            : attributes
                            )
                            [ Div [ HostRef handleTriggerRef ] []
                            , Div [ HostRef handleStickyRef, StyleList computedStyles ] children
                            ]

                }

instance HasProp As (Sticky ms) where
    type Prop As (Sticky ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f s = s { as = f }

instance HasProp Attributes (Sticky ms) where
    type Prop Attributes (Sticky ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs s = s { attributes = cs }

instance HasProp Children (Sticky ms) where
    type Prop Children (Sticky ms) = [View ms]
    getProp _ = children
    setProp _ cs s = s { children = cs }

instance HasProp Classes (Sticky ms) where
    type Prop Classes (Sticky ms) = [Txt]
    getProp _ = classes
    setProp _ cs s = s { classes = cs }

instance HasProp Active (Sticky ms) where
    type Prop Active (Sticky ms) = Bool
    getProp _ = active
    setProp _ a s = s { active = a }

instance HasProp BottomOffset (Sticky ms) where
    type Prop BottomOffset (Sticky ms) = Double
    getProp _ = bottomOffset
    setProp _ bo s = s { bottomOffset = bo }

instance HasProp Context (Sticky ms) where
    type Prop Context (Sticky ms) = Maybe JSV
    getProp _ = context
    setProp _ c s = s { context = c }

instance HasProp Offset (Sticky ms) where
    type Prop Offset (Sticky ms) = Double
    getProp _ = offset
    setProp _ o s = s { offset = o }

instance HasProp OnBottom (Sticky ms) where
    type Prop OnBottom (Sticky ms) = Ef ms IO ()
    getProp _ = onBottom
    setProp _ ob s = s { onBottom = ob }

instance HasProp OnStick (Sticky ms) where
    type Prop OnStick (Sticky ms) = Ef ms IO ()
    getProp _ = onStick
    setProp _ os s = s { onStick = os }

instance HasProp OnTop (Sticky ms) where
    type Prop OnTop (Sticky ms) = Ef ms IO ()
    getProp _ = onTop
    setProp _ ot s = s { onTop = ot }

instance HasProp OnUnstick (Sticky ms) where
    type Prop OnUnstick (Sticky ms) = Ef ms IO ()
    getProp _ = onUnstick
    setProp _ ou s = s { onUnstick = ou }

instance HasProp Pushing (Sticky ms) where
    type Prop Pushing (Sticky ms) = Bool
    getProp _ = pushing
    setProp _ p s = s { pushing = p }

instance HasProp ScrollContext (Sticky ms) where
    type Prop ScrollContext (Sticky ms) = Maybe JSV
    getProp _ = scrollContext
    setProp _ sc s = s { scrollContext = sc }
