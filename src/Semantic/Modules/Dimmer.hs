{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Dimmer where

import Data.IORef
import GHC.Generics as G
import Pure.View
import Pure.Lifted (Node(..))

import Semantic.Utils

import Semantic.Addons.Portal

import Semantic.Properties.CloseOnEscape
import Semantic.Properties.CloseOnDocumentClick
import Semantic.Properties.OpenOnTriggerClick
import Semantic.Properties.OnMount
import Semantic.Properties.OnUnmount
import Semantic.Properties.Open

import Semantic.Properties.Children

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
                                    ( ClassList cs
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
                                    & NoCloseOnEscape & NoCloseOnDocumentClick & NoOpenOnTriggerClick 
                                    & OnMount handlePortalMount & OnUnmount handlePortalUnmount 
                                    & Open active
                                    & Children [ dimmer ]
                            else dimmer
                    }
                
                    