module Semantic.Modules.Sidebar where

import Control.Concurrent
import Data.IORef
import GHC.Generics as G
import Pure.View hiding (animation,direction,visible,width)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasAnimationProp(..), pattern Animation
  , HasDirectionProp(..), pattern Direction
  , HasVisibleProp(..), pattern Visible
  , HasWidthProp(..), pattern Width
  , HasAnimationDurationProp(..), pattern AnimationDuration, AnimationDuration(..)
  , HasDimmedProp(..), pattern Dimmed
  )

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

instance HasAsProp (Pushable ms) where
    type AsProp (Pushable ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sp = sp { as = a }

instance HasAttributesProp (Pushable ms) where
    type Attribute (Pushable ms) = Feature ms
    getAttributes = attributes
    setAttributes as sp = sp { attributes = as }

instance HasChildrenProp (Pushable ms) where
    type Child (Pushable ms) = View ms
    getChildren = children
    setChildren cs sp = sp { children = cs }

instance HasClassesProp (Pushable ms) where
    getClasses = classes
    setClasses cs sp = sp { classes = cs }

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

instance HasAsProp (Pusher ms) where
    type AsProp (Pusher ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sp = sp { as = a }

instance HasAttributesProp (Pusher ms) where
    type Attribute (Pusher ms) = Feature ms
    getAttributes = attributes
    setAttributes as sp = sp { attributes = as }

instance HasChildrenProp (Pusher ms) where
    type Child (Pusher ms) = View ms
    getChildren = children
    setChildren cs sp = sp { children = cs }

instance HasClassesProp (Pusher ms) where
    getClasses = classes
    setClasses cs sp = sp { classes = cs }

instance HasDimmedProp (Pusher ms) where
    getDimmed = dimmed
    setDimmed d sp = sp { dimmed = d }
