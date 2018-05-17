{-# LANGUAGE UndecidableInstances #-}
module Semantic.Dimmer
  ( module Properties
  , module Tools
  , Dimmer(..), pattern Dimmer
  , Dimmable(..), pattern Dimmable
  ) where

import Data.IORef
import GHC.Generics as G
import Pure.View hiding (active,disabled,onClick,simple)
import Pure.Lifted (Node(..))

import Semantic.Utils

import Semantic.Portal

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern CloseOnEscape, CloseOnEscape(..)
  , pattern CloseOnDocumentClick, CloseOnDocumentClick(..)
  , pattern OpenOnTriggerClick, OpenOnTriggerClick(..)
  , pattern OnMount, OnMount(..)
  , pattern OnUnmount, OnUnmount(..)
  , pattern Open, Open(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
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
import Pure.Data.Default as Tools

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
    render d =
        Component "Semantic.Modules.Dimmer" d $ \self ->
            let
                handlePortalMount = do
                    addBodyClass "dimmed"
                    addBodyClass "dimmable"

                handlePortalUnmount = do
                    removeBodyClass "dimmed"
                    removeBodyClass "dimmable"

                handleClick e = do
                    Dimmer_ {..} <- getProps self
                    center <- liftIO $ readIORef =<< getState self
                    onClick
                    case center of
                        Nothing       -> onClickOutside
                        Just (Node c) -> do
                            targetNotCenter <- liftIO $ unequalTargets c (evtTarget e)
                            inside          <- liftIO $ c `contains`     (evtTarget e)
                            unless (targetNotCenter && inside) onClickOutside

                handleCenterRef n = do
                    centerRef <- getState self
                    writeIORef centerRef (Just n)
                    return Nothing

            in
                def
                    { construct = do
                        centerRef <- newIORef Nothing
                        return centerRef
                    , renderer = \Dimmer_ {..} _ ->
                        let cs =
                                ( "ui"
                                : active # "active transition visible"
                                : disabled # "disabled"
                                : inverted # "inverted"
                                : page # "page"
                                : simple # "simple"
                                : "dimmer"
                                )

                            dimmer =
                                as
                                    : On "click" def (return . Just . handleClick)
                                    : attributes
                                    )
                                    ( children # [
                                        Div [ ClassList [ "content" ] ]
                                            [ Div [ ClassList [ "center" ], HostRef handleCenterRef ]
                                                children
                                            ]
                                        ]
                                    )

                        in if page
                            then Portal $ def
                                    & CloseOnEscape False & CloseOnDocumentClick False & OpenOnTriggerClick False
                                    & OnMount handlePortalMount & OnUnmount handlePortalUnmount
                                    & Open active
                                    & Children [ dimmer ]
                            else dimmer
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
    render Dimmable_ {..} =
        let
            cs =
                ( blurring # "blurring"
                : dimmed # "dimmed"
                : "dimmable"
                )
        in
            as
                : attributes
                )
                children

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
