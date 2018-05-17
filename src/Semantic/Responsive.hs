module Semantic.Responsive
  ( module Properties
  , module Tools
  , Responsive(..), pattern Responsive
  , pattern OnlyMobile, pattern OnlyTablet
  , pattern OnlyComputer, pattern OnlyLargeScreen, pattern OnlyWidescreen
  )where

import Data.IORef
import GHC.Generics as G
import Pure.View hiding (minWidth,maxWidth)
import Pure.DOM (addAnimation)
import Pure.Lifted (Win(..),Node(..),getWindow)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern MinWidth, MinWidth(..)
  , pattern MaxWidth, MaxWidth(..)
  , pattern FireOnMount, FireOnMount(..)
  , pattern OnUpdate, OnUpdate(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Responsive = Responsive_
    { as          :: Features -> [View] -> View
    , attributes  :: Features
    , children    :: [View]
    , fireOnMount :: Bool
    , maxWidth    :: Int
    , minWidth    :: Int
    , onUpdate    :: IO ()
    } deriving (Generic)

instance Default Responsive where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Responsive :: Responsive -> Responsive
pattern Responsive r = r

pattern OnlyMobile :: Responsive -> Responsive
pattern OnlyMobile r = (MinWidth 320 (MaxWidth 767 r))

pattern OnlyTablet :: Responsive -> Responsive
pattern OnlyTablet r = (MinWidth 768 (MaxWidth 991 r))

pattern OnlyComputer :: Responsive -> Responsive
pattern OnlyComputer r = (MinWidth 992 r)

pattern OnlyLargeScreen :: Responsive -> Responsive
pattern OnlyLargeScreen r = (MinWidth 1200 (MaxWidth 1919 r))

pattern OnlyWidescreen :: Responsive -> Responsive
pattern OnlyWidescreen r = (MinWidth 1920 r)

data ResponsiveState = RS
    { width   :: Int
    , handler :: IORef (IO ())
    , ticking :: IORef Bool
    }

instance Pure Responsive where
    render r =
        Component "Semantic.Addons.Responsive" r $ \self ->
            let
                handleResize = liftIO $ do
                    RS {..} <- getState self
                    tick <- readIORef ticking
                    unless tick $ do
                        writeIORef ticking True
                        void $ addAnimation handleUpdate

                handleUpdate = do
                    Responsive_ {..} <- getProps self
                    RS {..} <- getState self
                    writeIORef ticking False
                    w <- innerWidth
                    setState self $ \_ RS {..} -> RS { width = w, .. }
                    void $ parent self onUpdate

            in def
                { construct = RS <$> innerWidth <*> newIORef def <*> newIORef def

                , mounted = do
                    Responsive_ {..} <- getProps self
                    RS {..} <- getState self
                    Win w <- getWindow
                    h <- onRaw (Node w) "resize" def (\_ _ -> handleResize)
                    writeIORef handler h
                    fireOnMount # handleUpdate

                , unmount = do
                    RS {..} <- getState self
                    join $ readIORef handler
                    writeIORef handler def

                , renderer = \Responsive_ {..} RS {..} ->
                     (width <= maxWidth && width >= minWidth) #
                        as attributes children

                }

instance HasProp As Responsive where
    type Prop As Responsive = Features -> [View] -> View
    getProp _ = as
    setProp _ a r = r { as = a }

instance HasFeatures Responsive where
    getFeatures = features
    setFeatures as r = r { features = as }

instance HasChildren Responsive where
    getChildren = children
    setChildren cs r = r { children = cs }


instance HasProp FireOnMount Responsive where
    type Prop FireOnMount Responsive = Bool
    getProp _ = fireOnMount
    setProp _ fom r = r { fireOnMount = fom }

instance HasProp MaxWidth Responsive where
    type Prop MaxWidth Responsive = Int
    getProp _ = maxWidth
    setProp _ mw r = r { maxWidth = mw }

instance HasProp MinWidth Responsive where
    type Prop MinWidth Responsive = Int
    getProp _ = minWidth
    setProp _ mw r = r { minWidth = mw }

instance HasProp OnUpdate Responsive where
    type Prop OnUpdate Responsive = IO ()
    getProp _ = onUpdate
    setProp _ ou r = r { onUpdate = ou }
