{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Dimmer (module Semantic.Modules.Dimmer, module Export) where

import Data.IORef
import GHC.Generics as G
import Pure.View hiding (active,disabled,onClick,simple)
import Pure.Lifted (Node(..))

import Semantic.Utils

import Semantic.Addons.Portal

import Semantic.Properties.CloseOnEscape
import Semantic.Properties.CloseOnDocumentClick
import Semantic.Properties.OpenOnTriggerClick
import Semantic.Properties.OnMount
import Semantic.Properties.OnUnmount
import Semantic.Properties.Open

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Disabled
import Semantic.Properties.OnClick
import Semantic.Properties.OnClickOutside
import Semantic.Properties.Inverted
import Semantic.Properties.Page
import Semantic.Properties.Simple

import Semantic.Modules.Dimmer.DimmerDimmable as Export

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
 
instance HasAsProp (Dimmer ms) where
    type AsProp (Dimmer ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a d = d { as = a }

instance HasAttributesProp (Dimmer ms) where
    type Attribute (Dimmer ms) = Feature ms
    getAttributes = attributes
    setAttributes as d = d { attributes = as }

instance HasChildrenProp (Dimmer ms) where
    type Child (Dimmer ms) = View ms
    getChildren = children
    setChildren cs d = d { children = cs }

instance HasClassesProp (Dimmer ms) where
    getClasses = classes
    setClasses cs d = d { classes = cs }

instance HasActiveProp (Dimmer ms) where
    getActive = active
    setActive a d = d { active = a }

instance HasDisabledProp (Dimmer ms) where
    getDisabled = disabled
    setDisabled d a = a { disabled = d }

instance HasOnClickProp (Dimmer ms) where
    type OnClickProp (Dimmer ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc d = d { onClick = oc }

instance HasOnClickOutsideProp (Dimmer ms) where
    type OnClickOutsideProp (Dimmer ms) = Ef ms IO ()
    getOnClickOutside = onClickOutside
    setOnClickOutside oco d = d { onClickOutside = oco }

instance HasInvertedProp (Dimmer ms) where
    getInverted = inverted
    setInverted i d = d { inverted = i }

instance HasPageProp (Dimmer ms) where
    getPage = page
    setPage p d = d { page = p }

instance HasSimpleProp (Dimmer ms) where
    getSimple = simple
    setSimple s d = d { simple = s }