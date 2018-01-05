module Semantic.Addons.Responsive where

import Data.IORef
import GHC.Generics as G
import Pure.View hiding (minWidth,maxWidth)
import Pure.DOM (addAnimation)
import Pure.Lifted (Win(..),Node(..),getWindow)

import Semantic.Utils

data Responsive ms = Responsive_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fireOnMount :: Bool
    , maxWidth :: Int
    , minWidth :: Int
    , onUpdate :: Ef ms IO ()
    } deriving (Generic)

instance Default (Responsive ms) where
    def = (G.to gdef) { as = Div }

pattern Responsive :: Typeable ms => Responsive ms -> View ms
pattern Responsive r = View r

data ResponsiveState = RS
    { width :: Int
    , handler :: IORef (IO ())
    , ticking :: IORef Bool
    }

instance Typeable ms => Pure Responsive ms where
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
    getOnUpdate = onUpdate
    setOnUpdate ou r = r { onUpdate = ou }