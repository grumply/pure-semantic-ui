module Semantic.Sidebar
  ( module Properties
  , module Tools
  , Sidebar(..), pattern Sidebar
  , Pushable(..), pattern Pushable
  , Pusher(..), pattern Pusher
  ) where

import Pure hiding (animation,direction,visible,width)

import Control.Concurrent
import Control.Monad
import Data.Foldable
import Data.IORef
import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
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
        { as = \fs cs -> Div & Features fs & Children cs
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
    view =
        LibraryComponentIO $ \self ->
            let
            in def
                { construct = SS def <$> newIORef def

                , receive = \newprops oldstate -> do
                    oldprops <- ask self
                    if visible newprops /= visible oldprops then do
                        SS {..} <- get self
                        traverse_ killThread =<< readIORef animator
                        tid <- forkIO $ do
                            let d = case duration newprops of
                                        Uniform u   -> u
                                        Skewed {..} -> visible newprops ? show $ hide
                            threadDelay (1000 * d)
                            modify_ self $ \_ SS {..} -> SS { animating = False, .. }
                        writeIORef animator (Just tid)
                        return oldstate { animating = True }
                    else
                        return oldstate

                , render = \Sidebar_ {..} SS {..} ->
                    let
                        cs =
                            [ "ui"
                            , animation
                            , direction
                            , width
                            , animating # "animating"
                            , visible # "visible"
                            , "sidebar"
                            ]
                    in
                        as (features & Classes cs) children
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
    view Pushable_ {..} = as (features & Class "pushable") children

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
    view Pusher_ {..} =
        let
            cs =
                [ "pusher"
                , dimmed # "dimmed"
                ]
        in
            as (features & Classes cs) children

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
