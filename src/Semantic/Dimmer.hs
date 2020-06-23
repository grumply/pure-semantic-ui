{-# LANGUAGE UndecidableInstances #-}
module Semantic.Dimmer
  ( module Properties
  , module Tools
  , Dimmer(..), pattern Dimmer
  , Dimmable(..), pattern Dimmable
  ) where

import Pure hiding (not,disabled,active,(#))

import Control.Monad (unless)
import Data.Coerce
import Data.IORef
import Data.List as List
import GHC.Generics as G

import Semantic.Utils

import Semantic.Proxy

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern CloseOnEscape, CloseOnEscape(..)
  , pattern CloseOnDocumentClick, CloseOnDocumentClick(..)
  , pattern OpenOnTriggerClick, OpenOnTriggerClick(..)
  , pattern Open, Open(..)
  , pattern As, As(..)
  , pattern Active, Active(..)
  , pattern Disabled, Disabled(..)
  , pattern OnClick, OnClick(..)
  , pattern OnClickOutside, OnClickOutside(..)
  , pattern Inverted, Inverted(..)
  , pattern Page, Page(..)
  , pattern Simple, Simple(..)
  , pattern Blurring, Blurring(..)
  , pattern Dimmed, Dimmed(..)
  )

import Data.Function as Tools ((&))

data Dimmer = Dimmer_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , disabled :: Bool
    , onClick :: IO ()
    , onClickOutside :: IO ()
    , inverted :: Bool
    , page :: Bool
    , simple :: Bool
    } deriving (Generic)

instance Default Dimmer where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Dimmer :: Dimmer -> Dimmer
pattern Dimmer d = d

instance Pure Dimmer where
    view =
        Component $ \self ->
            let
                handlePortalMount = do
                    addBodyClass "dimmed"
                    addBodyClass "dimmable"

                handlePortalUnmount = do
                    removeBodyClass "dimmed"
                    removeBodyClass "dimmable"

                handleClick e = do
                    Dimmer_ {..} <- ask self
                    center       <- readIORef =<< get self
                    onClick
                    case center of
                        Nothing       -> onClickOutside
                        Just (Node c) -> do
                            targetNotCenter <- unequalTargets c (evtTarget e)
                            inside          <- c `contains`     (evtTarget e)
                            unless (targetNotCenter && inside) onClickOutside

                handleCenterRef n = do
                    centerRef <- get self
                    writeIORef centerRef (Just n)

            in
                def
                    { construct = do
                        centerRef <- newIORef Nothing
                        return centerRef

                    , render = \Dimmer_ {..} _ ->
                        let cs =
                                [ "ui"
                                , active # "active transition visible"
                                , disabled # "disabled"
                                , inverted # "inverted"
                                , page # "page"
                                , simple # "simple"
                                , "dimmer"
                                ]

                            dimmer =
                                Proxy def <| OnMount handlePortalMount . OnUnmounted handlePortalUnmount |>
                                  [ as (features & Classes cs & Pure.OnClick handleClick)
                                      ( (not $ List.null children)
                                          ? [ Div <| Class "content" |>
                                                [ Div <| Class "center" . Lifecycle (HostRef handleCenterRef) |>
                                                    children
                                                ]
                                            ]
                                          $ [ ]
                                      )
                                  ]

                            view
                              | active && page = Portal (coerce Pure.body) dimmer
                              | active         = dimmer
                              | otherwise      = Null

                        in
                            view
                    }

instance HasProp As Dimmer where
    type Prop As Dimmer = Features -> [View] -> View
    getProp _ = as
    setProp _ a d = d { as = a }

instance HasFeatures Dimmer where
    getFeatures = features
    setFeatures as d = d { features = as }

instance HasChildren Dimmer where
    getChildren = children
    setChildren cs d = d { children = cs }

instance HasProp Active Dimmer where
    type Prop Active Dimmer = Bool
    getProp _ = active
    setProp _ a d = d { active = a }

instance HasProp Disabled Dimmer where
    type Prop Disabled Dimmer = Bool
    getProp _ = disabled
    setProp _ d a = a { disabled = d }

instance HasProp OnClick Dimmer where
    type Prop OnClick Dimmer = IO ()
    getProp _ = onClick
    setProp _ oc d = d { onClick = oc }

instance HasProp OnClickOutside Dimmer where
    type Prop OnClickOutside Dimmer = IO ()
    getProp _ = onClickOutside
    setProp _ oco d = d { onClickOutside = oco }

instance HasProp Inverted Dimmer where
    type Prop Inverted Dimmer = Bool
    getProp _ = inverted
    setProp _ i d = d { inverted = i }

instance HasProp Page Dimmer where
    type Prop Page Dimmer = Bool
    getProp _ = page
    setProp _ p d = d { page = p }

instance HasProp Simple Dimmer where
    type Prop Simple Dimmer = Bool
    getProp _ = simple
    setProp _ s d = d { simple = s }

data Dimmable = Dimmable_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , blurring :: Bool
    , dimmed :: Bool
    } deriving (Generic)

instance Default Dimmable where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Dimmable :: Dimmable -> Dimmable
pattern Dimmable dd = dd

instance Pure Dimmable where
    view Dimmable_ {..} =
        let
            cs =
                [ blurring # "blurring"
                , dimmed # "dimmed"
                , "dimmable"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Dimmable where
    type Prop As Dimmable = Features -> [View] -> View
    getProp _ = as
    setProp _ a dd = dd { as = a }

instance HasFeatures Dimmable where
    getFeatures = features
    setFeatures as dd = dd { features = as }

instance HasChildren Dimmable where
    getChildren = children
    setChildren cs dd = dd { children = cs }

instance HasProp Blurring Dimmable where
    type Prop Blurring Dimmable = Bool
    getProp _ = blurring
    setProp _ b dd = dd { blurring = b }

instance HasProp Dimmed Dimmable where
    type Prop Dimmed Dimmable = Bool
    getProp _ = dimmed
    setProp _ d dd = dd { dimmed = d }
