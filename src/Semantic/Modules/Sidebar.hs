module Semantic.Modules.Sidebar where

import Control.Concurrent
import Data.IORef
import GHC.Generics as G
import Pure.View hiding (animation,direction,visible,width)

import Semantic.Utils

import Semantic.Properties.AnimationDuration

data Sidebar ms = Sidebar_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , animation :: Txt
    , direction :: Txt
    , duration :: AnimationDuration
    , visible :: Bool
    , width :: Txt
    } deriving (Generic)

instance Default (Sidebar ms) where
    def = (G.to gdef) 
        { as = Div
        , direction = "left" 
        , duration = Uniform 500
        }

pattern Sidebar :: Typeable ms => Sidebar ms -> View ms
pattern Sidebar sb = View sb

data SidebarState = SS
    { animating :: Bool
    , animator :: IORef (Maybe ThreadId)
    }

instance Typeable ms => Pure Sidebar ms where
    render sb =
        Component "Semantic.Modules.Sidebar" sb $ \self ->
            let
            in def
                { construct = SS def <$> newIORef def

                , receiveProps = \newprops oldstate -> do
                    oldprops <- getProps self
                    if visible newprops /= visible oldprops then do
                        SS {..} <- getState self
                        traverse_ killThread =<< readIORef animator
                        tid <- forkIO $ do
                            let d = case duration newprops of
                                        Uniform u   -> u
                                        Skewed {..} -> visible newprops ? show $ hide
                            threadDelay (1000 * d)
                            void $ setState self $ \_ SS {..} -> SS { animating = False, .. }
                        writeIORef animator (Just tid)
                        return oldstate { animating = True }
                    else
                        return oldstate

                , renderer = \Sidebar_ {..} SS {..} ->
                    let
                        cs =
                            ( "ui"
                            : animation
                            : direction
                            : width
                            : animating # "animating"
                            : visible # "visible"
                            : "sidebar"
                            : classes
                            )
                    in
                        as 
                            ( mergeClasses $ ClassList cs
                            : attributes 
                            )
                            children
                }
