{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Sticky where

import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.Lifted (window,body,IsJSV(..),JSV,Node(..),Element(..))
import Pure.View hiding (active,bottom,offset,top,round)
import Pure.DOM (addAnimation)

import Semantic.Utils hiding (body)

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.BottomOffset
import Semantic.Properties.Context
import Semantic.Properties.Offset
import Semantic.Properties.OnBottom
import Semantic.Properties.OnStick
import Semantic.Properties.OnTop
import Semantic.Properties.OnUnstick
import Semantic.Properties.Pushing
import Semantic.Properties.ScrollContext

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
                        Nothing -> return (0,0,0,0)

                    cr <- boundingRect (Element $ fromMaybe (toJSV body) context)
                    
                    msr <- readIORef stickyRef
                    sr <- case msr of
                        Just sr -> boundingRect (Element sr)
                        Nothing -> return (0,0,0,0)
                    
                    return (tr,cr,sr)

                upd (triggerRect,contextRect,stickyRect) = do
                    s <- getProps self
                    ss@SS {..} <- getState self
                    writeIORef ticking False
                    ih <- innerHeight
                    (w triggerRect /= triggerWidth) #
                        void (setState self $ \_ SS {..} -> SS { triggerWidth = w triggerRect, .. })
                    void (upd' s ss ih)
                    where
                        b (b_,_,_,_) = b_
                        h (_,h_,_,_) = h_
                        t (_,_,t_,_) = t_
                        w (_,_,_,w_) = w_

                        upd' Sticky_ {..} SS {..} (fromIntegral -> ih)
                            | isPushing && t stickyRect <= t triggerRect     = stickToContextTop
                            | isPushing && b contextRect + bottomOffset > ih = stickToScreenBottom
                            | isPushing                                      = stickToContextBottom
                            | h stickyRect > ih && t contextRect > 0         = stickToContextTop
                            | h stickyRect > ih && b contextRect < ih        = stickToContextBottom
                            | t triggerRect < offset && 
                              h stickyRect + offset >= b contextRect         = stickToContextBottom
                            | t triggerRect < offset                         = stickToScreenTop
                            | True                                           = stickToContextTop
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
                                        { top = Just (b contextRect - h stickyRect)
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
                    let changed = active oldprops /= active newprops
                    if | changed && active newprops -> handleUpdate >> addListeners newprops >> return oldstate
                       | changed                    -> removeListeners >> return oldstate { isSticking = False }
                       | True                       -> return oldstate     

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


instance HasAsProp (Sticky ms) where
    type AsProp (Sticky ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f s = s { as = f }

instance HasAttributesProp (Sticky ms) where
    type Attribute (Sticky ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs s = s { attributes = cs }

instance HasChildrenProp (Sticky ms) where
    type Child (Sticky ms) = View ms
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasClassesProp (Sticky ms) where
    getClasses = classes
    setClasses cs s = s { classes = cs }

instance HasActiveProp (Sticky ms) where
    getActive = active
    setActive a s = s { active = a }

instance HasBottomOffsetProp (Sticky ms) where
    getBottomOffset = bottomOffset
    setBottomOffset bo s = s { bottomOffset = bo }

instance HasContextProp (Sticky ms) where
    type ContextProp (Sticky ms) = Maybe JSV
    getContext = context
    setContext c s = s { context = c }

instance HasOffsetProp (Sticky ms) where
    type OffsetProp (Sticky ms) = Double
    getOffset = offset
    setOffset o s = s { offset = o }

instance HasOnBottomProp (Sticky ms) where
    type OnBottomProp (Sticky ms) = Ef ms IO ()
    getOnBottom = onBottom
    setOnBottom ob s = s { onBottom = ob }

instance HasOnStickProp (Sticky ms) where
    type OnStickProp (Sticky ms) = Ef ms IO ()
    getOnStick = onStick
    setOnStick os s = s { onStick = os }

instance HasOnTopProp (Sticky ms) where
    type OnTopProp (Sticky ms) = Ef ms IO ()
    getOnTop = onTop
    setOnTop ot s = s { onTop = ot }

instance HasOnUnstickProp (Sticky ms) where
    type OnUnstickProp (Sticky ms) = Ef ms IO ()
    getOnUnstick = onUnstick
    setOnUnstick ou s = s { onUnstick = ou }

instance HasPushingProp (Sticky ms) where
    getPushing = pushing
    setPushing p s = s { pushing = p }

instance HasScrollContextProp (Sticky ms) where
    getScrollContext = scrollContext
    setScrollContext sc s = s { scrollContext = sc }