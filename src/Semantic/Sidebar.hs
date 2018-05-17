module Semantic.Sidebar
  ( module Properties
  , module Tools
  , Sidebar(..), pattern Sidebar
  , Pushable(..), pattern Pushable
  , Pusher(..), pattern Pusher
  ) where

import Control.Concurrent
import Data.IORef
import GHC.Generics as G
import Pure.View hiding (animation,direction,visible,width)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Animation, Animation(..)
  , pattern Direction, Direction(..)
  , pattern Visible, Visible(..)
  , pattern Width, Width(..)
  , pattern AnimationDuration, AnimationDuration(..)
  , pattern Dimmed, Dimmed(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Sidebar = Sidebar_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , animation :: Txt
    , direction :: Txt
    , duration :: AnimationDuration
    , visible :: Bool
    , width :: Txt
    } deriving (Generic)

instance Default Sidebar where
    def = (G.to gdef)
        { as = Div
        , direction = "left"
        , duration = Uniform 500
        }

pattern Sidebar :: Sidebar -> Sidebar
pattern Sidebar sb = sb

data SidebarState = SS
    { animating :: Bool
    , animator :: IORef (Maybe ThreadId)
    }

instance Pure Sidebar where
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
                            )
                    in
                        as
                            : attributes
                            )
                            children
                }

instance HasProp As Sidebar where
    type Prop As Sidebar = Features -> [View] -> View
    getProp _ = as
    setProp _ f sb = sb { as = f }

instance HasFeatures Sidebar where
    getFeatures = features
    setFeatures cs sb = sb { features = cs }

instance HasChildren Sidebar where
    getChildren = children
    setChildren cs sb = sb { children = cs }


instance HasProp Animation Sidebar where
    type Prop Animation Sidebar = Txt
    getProp _ = animation
    setProp _ a sb = sb { animation = a }

instance HasProp Direction Sidebar where
    type Prop Direction Sidebar = Txt
    getProp _ = direction
    setProp _ d sb = sb { direction = d }

instance HasProp AnimationDuration Sidebar where
    type Prop AnimationDuration Sidebar = AnimationDuration
    getProp _ = duration
    setProp _ d sb = sb { duration = d }

instance HasProp Visible Sidebar where
    type Prop Visible Sidebar = Bool
    getProp _ = visible
    setProp _ v sb = sb { visible = v }

instance HasProp Width Sidebar where
    type Prop Width Sidebar = Txt
    getProp _ = width
    setProp _ w sb = sb { width = w }

data Pushable = Pushable_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Pushable where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Pushable :: Pushable -> Pushable
pattern Pushable sp = sp

instance Pure Pushable where
    render Pushable_ {..} =
        let
            cs =
                ( "pushable"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Pushable where
    type Prop As Pushable = Features -> [View] -> View
    getProp _ = as
    setProp _ a sp = sp { as = a }

instance HasFeatures Pushable where
    getFeatures = features
    setFeatures as sp = sp { features = as }

instance HasChildren Pushable where
    getChildren = children
    setChildren cs sp = sp { children = cs }


data Pusher = Pusher_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , dimmed :: Bool
    } deriving (Generic)

instance Default Pusher where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Pusher :: Pusher -> Pusher
pattern Pusher sp = sp

instance Pure Pusher where
    render Pusher_ {..} =
        let
            cs =
                ( "pusher"
                : dimmed # "dimmed"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Pusher where
    type Prop As Pusher = Features -> [View] -> View
    getProp _ = as
    setProp _ a sp = sp { as = a }

instance HasFeatures Pusher where
    getFeatures = features
    setFeatures as sp = sp { features = as }

instance HasChildren Pusher where
    getChildren = children
    setChildren cs sp = sp { children = cs }


instance HasProp Dimmed Pusher where
    type Prop Dimmed Pusher = Bool
    getProp _ = dimmed
    setProp _ d sp = sp { dimmed = d }
