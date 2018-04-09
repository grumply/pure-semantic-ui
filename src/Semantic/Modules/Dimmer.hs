{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Dimmer
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

import Semantic.Addons.Portal

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

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
  , pattern Classes, Classes(..)
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

data Dimmer ms = Dimmer_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Ef ms IO ()
    , onClickOutside :: Ef ms IO ()
    , inverted :: Bool
    , page :: Bool
    , simple :: Bool
    } deriving (Generic)

instance Default (Dimmer ms) where
    def = (G.to gdef) { as = Div }

pattern Dimmer :: VC ms => Dimmer ms -> View ms
pattern Dimmer d = View d

instance VC ms => Pure Dimmer ms where
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
                                : classes
                                )

                            dimmer =
                                as
                                    ( mergeClasses $ ClassList cs
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

instance HasProp As (Dimmer ms) where
    type Prop As (Dimmer ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a d = d { as = a }

instance HasProp Attributes (Dimmer ms) where
    type Prop Attributes (Dimmer ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as d = d { attributes = as }

instance HasProp Children (Dimmer ms) where
    type Prop Children (Dimmer ms) = [View ms]
    getProp _ = children
    setProp _ cs d = d { children = cs }

instance HasProp Classes (Dimmer ms) where
    type Prop Classes (Dimmer ms) = [Txt]
    getProp _ = classes
    setProp _ cs d = d { classes = cs }

instance HasProp Active (Dimmer ms) where
    type Prop Active (Dimmer ms) = Bool
    getProp _ = active
    setProp _ a d = d { active = a }

instance HasProp Disabled (Dimmer ms) where
    type Prop Disabled (Dimmer ms) = Bool
    getProp _ = disabled
    setProp _ d a = a { disabled = d }

instance HasProp OnClick (Dimmer ms) where
    type Prop OnClick (Dimmer ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc d = d { onClick = oc }

instance HasProp OnClickOutside (Dimmer ms) where
    type Prop OnClickOutside (Dimmer ms) = Ef ms IO ()
    getProp _ = onClickOutside
    setProp _ oco d = d { onClickOutside = oco }

instance HasProp Inverted (Dimmer ms) where
    type Prop Inverted (Dimmer ms) = Bool
    getProp _ = inverted
    setProp _ i d = d { inverted = i }

instance HasProp Page (Dimmer ms) where
    type Prop Page (Dimmer ms) = Bool
    getProp _ = page
    setProp _ p d = d { page = p }

instance HasProp Simple (Dimmer ms) where
    type Prop Simple (Dimmer ms) = Bool
    getProp _ = simple
    setProp _ s d = d { simple = s }

data Dimmable ms = Dimmable_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , blurring :: Bool
    , dimmed :: Bool
    } deriving (Generic)

instance Default (Dimmable ms) where
    def = (G.to gdef) { as = Div }

pattern Dimmable :: Dimmable ms -> View ms
pattern Dimmable dd = View dd

instance Pure Dimmable ms where
    render Dimmable_ {..} =
        let
            cs =
                ( blurring # "blurring"
                : dimmed # "dimmed"
                : "dimmable"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Dimmable ms) where
    type Prop As (Dimmable ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a dd = dd { as = a }

instance HasProp Attributes (Dimmable ms) where
    type Prop Attributes (Dimmable ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as dd = dd { attributes = as }

instance HasProp Children (Dimmable ms) where
    type Prop Children (Dimmable ms) = [View ms]
    getProp _ = children
    setProp _ cs dd = dd { children = cs }

instance HasProp Classes (Dimmable ms) where
    type Prop Classes (Dimmable ms) = [Txt]
    getProp _ = classes
    setProp _ cs dd = dd { classes = cs }

instance HasProp Blurring (Dimmable ms) where
    type Prop Blurring (Dimmable ms) = Bool
    getProp _ = blurring
    setProp _ b dd = dd { blurring = b }

instance HasProp Dimmed (Dimmable ms) where
    type Prop Dimmed (Dimmable ms) = Bool
    getProp _ = dimmed
    setProp _ d dd = dd { dimmed = d }
