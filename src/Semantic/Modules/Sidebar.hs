module Semantic.Modules.Sidebar (module Semantic.Modules.Sidebar, module Export) where

import Control.Concurrent
import Data.IORef
import GHC.Generics as G
import Pure.View hiding (animation,direction,visible,width)

import Semantic.Utils

import Semantic.Properties.AnimationDuration

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Animation
import Semantic.Properties.Direction
import Semantic.Properties.Visible
import Semantic.Properties.Width

import Semantic.Modules.Sidebar.SidebarPusher as Export
import Semantic.Modules.Sidebar.SidebarPushable as Export

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

pattern Sidebar :: Sidebar ms -> View ms
pattern Sidebar sb = View sb

data SidebarState = SS
    { animating :: Bool
    , animator :: IORef (Maybe ThreadId)
    }

instance Pure Sidebar ms where
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

instance HasAsProp (Sidebar ms) where
    type AsProp (Sidebar ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sb = sb { as = f }

instance HasAttributesProp (Sidebar ms) where
    type Attribute (Sidebar ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs sb = sb { attributes = cs }

instance HasChildrenProp (Sidebar ms) where
    type Child (Sidebar ms) = View ms
    getChildren = children
    setChildren cs sb = sb { children = cs }

instance HasClassesProp (Sidebar ms) where
    getClasses = classes
    setClasses cs sb = sb { classes = cs }

instance HasAnimationProp (Sidebar ms) where
    getAnimation = animation
    setAnimation a sb = sb { animation = a }

instance HasDirectionProp (Sidebar ms) where
    getDirection = direction
    setDirection d sb = sb { direction = d }

instance HasAnimationDurationProp (Sidebar ms) where
    getAnimationDuration = duration
    setAnimationDuration d sb = sb { duration = d }

instance HasVisibleProp (Sidebar ms) where
    getVisible = visible
    setVisible v sb = sb { visible = v }

instance HasWidthProp (Sidebar ms) where
    type WidthProp (Sidebar ms) = Txt
    getWidth = width
    setWidth w sb = sb { width = w }