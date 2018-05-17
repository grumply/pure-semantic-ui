{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modal
  ( module Properties
  , module Tools
  , Modal(..), pattern Modal
  , Actions(..), pattern Actions
  , Content(..), pattern Content
  , Description(..), pattern Description
  , Header(..), pattern Header
  ) where

import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (active,round,addClass,trigger,OnClose,Content,Description,Header,Styles)
import Pure.DOM (addAnimation)
import Pure.Lifted (body,IsJSV(..),JSV,Node(..),Element(..))

import Semantic.Utils

import qualified Semantic.Portal as Portal

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern CloseOnDocumentClick, CloseOnDocumentClick(..)
  , pattern CloseOnRootNodeClick, CloseOnRootNodeClick(..)
  , pattern OpenOnTriggerClick, OpenOnTriggerClick(..)
  , pattern OnClose, OnClose(..)
  , pattern OnMount, OnMount(..)
  , pattern OnOpen, OnOpen(..)
  , pattern OnUnmount, OnUnmount(..)
  , pattern Open, Open(..)
  , pattern Trigger, Trigger(..)
  , pattern MountNode, MountNode(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Basic, Basic(..)
  , pattern CloseOnDimmerClick, CloseOnDimmerClick(..)
  , pattern DefaultOpen, DefaultOpen(..)
  , pattern DimmerType, DimmerType(..)
  , pattern Scrollable, Scrollable(..)
  , pattern Size, Size(..)
  , pattern Styles, Styles(..)
  , pattern WithPortal, WithPortal(..)
  , pattern IsImage, IsImage(..)
  , pattern Scrolling, Scrolling(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data Modal = Modal_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , basic :: Bool
    , closeOnDimmerClick :: Bool
    , closeOnDocumentClick :: Bool
    , defaultOpen :: Bool
    , dimmer :: Maybe Txt
    , mountNode :: Maybe JSV
    , onClose :: IO ()
    , onMount :: IO ()
    , onOpen :: IO ()
    , onUnmount :: IO ()
    , open :: Bool
    , scrollable :: Bool
    , size :: Txt
    , styles :: [(Txt,Txt)]
    , withPortal :: Portal.Portal -> Portal.Portal
    , trigger :: View
    } deriving (Generic)

instance Default Modal where
    def = (G.to gdef)
        { as = Div
        , dimmer = Just ""
        , closeOnDimmerClick = True
        , closeOnDocumentClick = True
        , withPortal = id
        }

pattern Modal :: Modal -> Modal
pattern Modal m = m

data ModalState =
    { topMargin :: Maybe Int
    , scrolling :: Maybe Bool
    , active :: Bool
    , ref :: IORef (Maybe JSV)
    , pendingAnimation :: IORef (IO ())
    }

instance VC => Pure Modal where
    render m =
        Component "Semantic.Modules.Modal" m $ \self ->
            let
                getMountNode = do
                    Modal_ {..} <- getProps self
                    return $ fromMaybe (toJSV body) mountNode

                handleRef (Node n) = do
                    {..} <- getState self
                    writeIORef ref (Just n)
                    return Nothing

                handleClose = do
                    Modal_ {..} <- getProps self
                    onClose
                    void $ setState self $ \_ {..} ->
                        { active = False, .. }

                handleOpen _ = do
                    Modal_ {..} <- getProps self
                    onOpen
                    void $ setState self $ \_ {..} ->
                        { active = True, .. }

                handlePortalMount = do
                    Modal_ {..} <- getProps self
                    setState self $ \_ {..} ->
                        { scrolling = Just False, .. }
                    liftIO setPositionAndClassNames
                    onMount

                handlePortalUnmount = do
                    Modal_ {..} <- getProps self
                        {..} <- getState self
                    n <- getMountNode
                    traverse_ (removeClass n) ["blurring","dimmable","dimmed","scrolling"]
                    writeIORef pendingAnimation def

                setPositionAndClassNames = do
                    Modal_ {..} <- getProps self
                        {..} <- getState self
                    n           <- getMountNode

                    dimmer # traverse_ (addClass n) ["dimmable","dimmed"]
                    (dimmer == Just "blurring") # addClass n "blurring"

                    mr <- readIORef ref

                    for_ mr $ \r -> do
                      BR { brHeight = h } <- boundingRect (Element r)

                      ih <- innerHeight

                      let topMargin' = negate (round (h / 2))
                          scrolling' = h >= fromIntegral ih

                          scrollingChange = scrolling /= Just scrolling'
                          topMarginChange = topMargin /= Just topMargin'

                      scrollingChange #
                        (scrolling' ? addClass n $ removeClass n)
                          "scrolling"

                      (scrollingChange || topMarginChange) #
                        setState self $ \_ {..} ->
                          { topMargin = Just topMargin'
                             , scrolling = Just scrolling'
                             , ..
                             }

                    writeIORef pendingAnimation setPositionAndClassNames
                    void $ addAnimation (join $ readIORef pendingAnimation)

            in def
                { construct = do
                    Modal_ {..} <- getProps self
                    def def (open || defaultOpen) <$> newIORef def <*> newIORef def
                , receiveProps = \newprops oldstate -> return $
                    (open newprops /= active oldstate)
                      ? oldstate { active = open newprops }
                      $ oldstate
                , unmount = do
                    Modal_ {..} <- getProps self
                    handlePortalUnmount
                    parent self onUnmount
                , renderer = \Modal_ {..} {..} ->
                    let
                            dimmer #
                                [ "ui"
                                , (dimmer == Just "inverted") # "inverted"
                                , "page modals dimmer transition visible active"
                                ]

                        renderContent =
                            let
                                ss = maybe styles (\mt -> (marginTop,pxs mt) : styles ) topMargin

                                cs =
                                    ( "ui"
                                    : size
                                    : basic # "basic"
                                    : (scrolling == Just True) # "scrolling"
                                    : "modal transition visible active"
                                    )

                                children' = flip map children $ \c ->
                                    case c of
                                        -- add ModalAction mapping here
                                        _ -> c

                            in
                                as
                                    : StyleList ss
                                    : HostRef handleRef
                                    : attributes
                                    )
                                    children

                    in Portal.Portal $ withPortal $ def
                        & (closeOnDocumentClick ? CloseOnDocumentClick True $ id)
                        & (closeOnDimmerClick   ? CloseOnRootNodeClick True $ id)
                        & Trigger trigger
                        & MountNode mountNode
                        & Open active
                        & OnClose handleClose
                        & OnMount handlePortalMount
                        & OnOpen handleOpen
                        & OnUnmount (liftIO handlePortalUnmount >> onUnmount)
                        & Children [ renderContent ]
                }

instance HasProp As Modal where
    type Prop As Modal = Features -> [View] -> View
    getProp _ = as
    setProp _ a m = m { as = a }

instance HasFeatures Modal where
    getFeatures = features
    setFeatures as m = m { features = as }

instance HasChildren Modal where
    getChildren = children
    setChildren cs m = m { children = cs }


instance HasProp Basic Modal where
    type Prop Basic Modal = Bool
    getProp _ = basic
    setProp _ b m = m { basic = b }

instance HasProp CloseOnDimmerClick Modal where
    type Prop CloseOnDimmerClick Modal = Bool
    getProp _ = closeOnDimmerClick
    setProp _ codc m = m { closeOnDimmerClick = codc }

instance HasProp CloseOnDocumentClick Modal where
    type Prop CloseOnDocumentClick Modal = Bool
    getProp _ = closeOnDocumentClick
    setProp _ codc m = m { closeOnDocumentClick = codc }

instance HasProp DefaultOpen Modal where
    type Prop DefaultOpen Modal = Bool
    getProp _ = defaultOpen
    setProp _ o m = m { defaultOpen = o }

instance HasProp DimmerType Modal where
    type Prop DimmerType Modal = Maybe Txt
    getProp _ = dimmer
    setProp _ d m = m { dimmer = d }

instance HasProp MountNode Modal where
    type Prop MountNode Modal = Maybe JSV
    getProp _ = mountNode
    setProp _ mn m = m { mountNode = mn }

instance HasProp OnClose Modal where
    type Prop OnClose Modal = IO ()
    getProp _ = onClose
    setProp _ oc m = m { onClose = oc }

instance HasProp OnMount Modal where
    type Prop OnMount Modal = IO ()
    getProp _ = onMount
    setProp _ om m = m { onMount = om }

instance HasProp OnOpen Modal where
    type Prop OnOpen Modal = IO ()
    getProp _ = onOpen
    setProp _ oo m = m { onOpen = oo }

instance HasProp OnUnmount Modal where
    type Prop OnUnmount Modal = IO ()
    getProp _ = onUnmount
    setProp _ ou m = m { onUnmount = ou }

instance HasProp Open Modal where
    type Prop Open Modal = Bool
    getProp _ = open
    setProp _ o m = m { open = o }

instance HasProp Scrollable Modal where
    type Prop Scrollable Modal = Bool
    getProp _ = scrollable
    setProp _ s m = m { scrollable = s }

instance HasProp Size Modal where
    type Prop Size Modal = Txt
    getProp _ = size
    setProp _ s m = m { size = s }

instance HasProp Styles Modal where
    type Prop Styles Modal = [(Txt,Txt)]
    getProp _ = styles
    setProp _ ss m = m { styles = ss }

instance HasProp WithPortal Modal where
    type Prop WithPortal Modal = Portal.Portal -> Portal.Portal
    getProp _ = withPortal
    setProp _ wp m = m { withPortal = wp }

instance HasProp Trigger Modal where
    type Prop Trigger Modal = View
    getProp _ = trigger
    setProp _ t m = m { trigger = t }

data Actions = Actions_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Actions where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Actions :: Actions -> Actions
pattern Actions ma = ma

instance Pure Actions where
    render Actions_ {..} =
        as
            : attributes
            )
            children

instance HasProp As Actions where
    type Prop As Actions = Features -> [View] -> View
    getProp _ = as
    setProp _ f ma = ma { as = f }

instance HasFeatures Actions where
    getFeatures = features
    setFeatures cs ma = ma { features = cs }

instance HasChildren Actions where
    getChildren = children
    setChildren cs ma = ma { children = cs }


data Content = Content_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , image :: Bool
    , scrolling :: Bool
    } deriving (Generic)

instance Default Content where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Content :: Content -> Content
pattern Content mc = mc

instance Pure Content where
    render Content_ {..} =
        let
                [ image # "image"
                , scrolling # "scrolling"
                , "content"
                ]

        in
            as
                : attributes
                )
                children

instance HasProp As Content where
    type Prop As Content = Features -> [View] -> View
    getProp _ = as
    setProp _ f mc = mc { as = f }

instance HasFeatures Content where
    getFeatures = features
    setFeatures cs mc = mc { features = cs }

instance HasChildren Content where
    getChildren = children
    setChildren cs mc = mc { children = cs }


instance HasProp IsImage Content where
    type Prop IsImage Content = Bool
    getProp _ = image
    setProp _ ii mc = mc { image = ii }

instance HasProp Scrolling Content where
    type Prop Scrolling Content = Bool
    getProp _ = scrolling
    setProp _ s mc = mc { scrolling = s }

data Description = Description_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Description where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Description :: Description -> Description
pattern Description md = md

instance Pure Description where
    render Description_ {..} =
        let
            cs =
                ( "description"
                )

        in
            as
                : attributes
                )
                children

instance HasProp As Description where
    type Prop As Description = Features -> [View] -> View
    getProp _ = as
    setProp _ f md = md { as = f }

instance HasFeatures Description where
    getFeatures = features
    setFeatures cs md = md { features = cs }

instance HasChildren Description where
    getChildren = children
    setChildren cs md = md { children = cs }


data Header = Header_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Header where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Header :: Header -> Header
pattern Header mh = mh

instance Pure Header where
    render Header_ {..} =
        let

        in
            as
                : attributes
                )
                children

instance HasProp As Header where
    type Prop As Header = Features -> [View] -> View
    getProp _ = as
    setProp _ f mh = mh { as = f }

instance HasFeatures Header where
    getFeatures = features
    setFeatures cs mh = mh { features = cs }

instance HasChildren Header where
    getChildren = children
    setChildren cs mh = mh { children = cs }

