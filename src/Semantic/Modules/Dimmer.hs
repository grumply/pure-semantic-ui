{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Dimmer where

import Data.IORef
import GHC.Generics as G
import Pure.View hiding (active,disabled,onClick,simple)
import Pure.Lifted (Node(..))

import Semantic.Utils

import Semantic.Addons.Portal

import Semantic.Properties as Properties
  ( HasCloseOnEscapeProp(..), pattern CloseOnEscape, pattern NoCloseOnEscape
  , HasCloseOnDocumentClickProp(..), pattern CloseOnDocumentClick, pattern NoCloseOnDocumentClick
  , HasOpenOnTriggerClickProp(..), pattern OpenOnTriggerClick, pattern NoOpenOnTriggerClick
  , HasOnMountProp(..), pattern OnMount
  , HasOnUnmountProp(..), pattern OnUnmount
  , HasOpenProp(..), pattern Open
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActiveProp(..), pattern Active
  , HasDisabledProp(..), pattern Disabled
  , HasOnClickProp(..), pattern OnClick
  , HasOnClickOutsideProp(..), pattern OnClickOutside
  , HasInvertedProp(..), pattern Inverted
  , HasPageProp(..), pattern Page
  , HasSimpleProp(..), pattern Simple
  , HasBlurringProp(..), pattern Blurring
  , HasDimmedProp(..), pattern Dimmed
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

instance HasAsProp (Dimmable ms) where
    type AsProp (Dimmable ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a dd = dd { as = a }

instance HasAttributesProp (Dimmable ms) where
    type Attribute (Dimmable ms) = Feature ms
    getAttributes = attributes
    setAttributes as dd = dd { attributes = as }

instance HasChildrenProp (Dimmable ms) where
    type Child (Dimmable ms) = View ms
    getChildren = children
    setChildren cs dd = dd { children = cs }

instance HasClassesProp (Dimmable ms) where
    getClasses = classes
    setClasses cs dd = dd { classes = cs }

instance HasBlurringProp (Dimmable ms) where
    getBlurring = blurring
    setBlurring b dd = dd { blurring = b }

instance HasDimmedProp (Dimmable ms) where
    getDimmed = dimmed
    setDimmed d dd = dd { dimmed = d }
