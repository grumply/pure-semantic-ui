module Semantic.Addons.Responsive
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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern MinWidth, MinWidth(..)
  , pattern MaxWidth, MaxWidth(..)
  , pattern FireOnMount, FireOnMount(..)
  , pattern OnUpdate, OnUpdate(..)
  )

data Responsive ms = Responsive_
    { as          :: [Feature ms] -> [View ms] -> View ms
    , attributes  :: [Feature ms]
    , children    :: [View ms]
    , classes     :: [Txt]
    , fireOnMount :: Bool
    , maxWidth    :: Int
    , minWidth    :: Int
    , onUpdate    :: Ef ms IO ()
    } deriving (Generic)

instance Default (Responsive ms) where
    def = (G.to gdef) { as = Div }

pattern Responsive :: Responsive ms -> View ms
pattern Responsive r = View r

pattern OnlyMobile :: Responsive ms -> View ms
pattern OnlyMobile r = View (MinWidth 320 (MaxWidth 767 r))

pattern OnlyTablet :: Responsive ms -> View ms
pattern OnlyTablet r = View (MinWidth 768 (MaxWidth 991 r))

pattern OnlyComputer :: Responsive ms -> View ms
pattern OnlyComputer r = View (MinWidth 992 r)

pattern OnlyLargeScreen :: Responsive ms -> View ms
pattern OnlyLargeScreen r = View (MinWidth 1200 (MaxWidth 1919 r))

pattern OnlyWidescreen :: Responsive ms -> View ms
pattern OnlyWidescreen r = View (MinWidth 1920 r)

data ResponsiveState = RS
    { width   :: Int
    , handler :: IORef (IO ())
    , ticking :: IORef Bool
    }

instance Pure Responsive ms where
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

instance HasProp As (Responsive ms) where
    type Prop As (Responsive ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a r = r { as = a }

instance HasProp Attributes (Responsive ms) where
    type Prop Attributes (Responsive ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as r = r { attributes = as }

instance HasProp Children (Responsive ms) where
    type Prop Children (Responsive ms) = [View ms]
    getProp _ = children
    setProp _ cs r = r { children = cs }

instance HasProp Classes (Responsive ms) where
    type Prop Classes (Responsive ms) = [Txt]
    getProp _ = classes
    setProp _ cs r = r { classes = cs }

instance HasProp FireOnMount (Responsive ms) where
    type Prop FireOnMount (Responsive ms) = Bool
    getProp _ = fireOnMount
    setProp _ fom r = r { fireOnMount = fom }

instance HasProp MaxWidth (Responsive ms) where
    type Prop MaxWidth (Responsive ms) = Int
    getProp _ = maxWidth
    setProp _ mw r = r { maxWidth = mw }

instance HasProp MinWidth (Responsive ms) where
    type Prop MinWidth (Responsive ms) = Int
    getProp _ = minWidth
    setProp _ mw r = r { minWidth = mw }

instance HasProp OnUpdate (Responsive ms) where
    type Prop OnUpdate (Responsive ms) = Ef ms IO ()
    getProp _ = onUpdate
    setProp _ ou r = r { onUpdate = ou }
