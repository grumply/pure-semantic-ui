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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Animation, Animation(..)
  , pattern Direction, Direction(..)
  , pattern Visible, Visible(..)
  , pattern Width, Width(..)
  , pattern AnimationDuration, AnimationDuration(..)
  , pattern Dimmed, Dimmed(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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

instance HasProp As (Sidebar ms) where
    type Prop As (Sidebar ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f sb = sb { as = f }

instance HasProp Attributes (Sidebar ms) where
    type Prop Attributes (Sidebar ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs sb = sb { attributes = cs }

instance HasProp Children (Sidebar ms) where
    type Prop Children (Sidebar ms) = [View ms]
    getProp _ = children
    setProp _ cs sb = sb { children = cs }

instance HasProp Classes (Sidebar ms) where
    type Prop Classes (Sidebar ms) = [Txt]
    getProp _ = classes
    setProp _ cs sb = sb { classes = cs }

instance HasProp Animation (Sidebar ms) where
    type Prop Animation (Sidebar ms) = Txt
    getProp _ = animation
    setProp _ a sb = sb { animation = a }

instance HasProp Direction (Sidebar ms) where
    type Prop Direction (Sidebar ms) = Txt
    getProp _ = direction
    setProp _ d sb = sb { direction = d }

instance HasProp AnimationDuration (Sidebar ms) where
    type Prop AnimationDuration (Sidebar ms) = AnimationDuration
    getProp _ = duration
    setProp _ d sb = sb { duration = d }

instance HasProp Visible (Sidebar ms) where
    type Prop Visible (Sidebar ms) = Bool
    getProp _ = visible
    setProp _ v sb = sb { visible = v }

instance HasProp Width (Sidebar ms) where
    type Prop Width (Sidebar ms) = Txt
    getProp _ = width
    setProp _ w sb = sb { width = w }

data Pushable ms = Pushable_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Pushable ms) where
    def = (G.to gdef) { as = Div }

pattern Pushable :: Pushable ms -> View ms
pattern Pushable sp = View sp

instance Pure Pushable ms where
    render Pushable_ {..} =
        let
            cs =
                ( "pushable"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Pushable ms) where
    type Prop As (Pushable ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sp = sp { as = a }

instance HasProp Attributes (Pushable ms) where
    type Prop Attributes (Pushable ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sp = sp { attributes = as }

instance HasProp Children (Pushable ms) where
    type Prop Children (Pushable ms) = [View ms]
    getProp _ = children
    setProp _ cs sp = sp { children = cs }

instance HasProp Classes (Pushable ms) where
    type Prop Classes (Pushable ms) = [Txt]
    getProp _ = classes
    setProp _ cs sp = sp { classes = cs }

data Pusher ms = Pusher_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , dimmed :: Bool
    } deriving (Generic)

instance Default (Pusher ms) where
    def = (G.to gdef) { as = Div }

pattern Pusher :: Pusher ms -> View ms
pattern Pusher sp = View sp

instance Pure Pusher ms where
    render Pusher_ {..} =
        let
            cs =
                ( "pusher"
                : dimmed # "dimmed"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Pusher ms) where
    type Prop As (Pusher ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sp = sp { as = a }

instance HasProp Attributes (Pusher ms) where
    type Prop Attributes (Pusher ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sp = sp { attributes = as }

instance HasProp Children (Pusher ms) where
    type Prop Children (Pusher ms) = [View ms]
    getProp _ = children
    setProp _ cs sp = sp { children = cs }

instance HasProp Classes (Pusher ms) where
    type Prop Classes (Pusher ms) = [Txt]
    getProp _ = classes
    setProp _ cs sp = sp { classes = cs }

instance HasProp Dimmed (Pusher ms) where
    type Prop Dimmed (Pusher ms) = Bool
    getProp _ = dimmed
    setProp _ d sp = sp { dimmed = d }
