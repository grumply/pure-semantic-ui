{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modal
  ( module Properties
  , module Tools
  , Modal(..), pattern Modal
  , Actions(..), pattern Actions
  , Content(..), pattern Content
  , Description(..), pattern Semantic.Modal.Description
  , Header(..), pattern Semantic.Modal.Header
  ) where

import Pure hiding (Open,Content_,Content)

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.Maybe
import GHC.Generics as G

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
  , pattern OnUnmounted, OnUnmounted(..)
  , pattern Open, Open(..)
  , pattern Trigger, Trigger(..)
  , pattern MountNode, MountNode(..)
  , pattern As, As(..)
  , pattern Basic, Basic(..)
  , pattern CloseOnDimmerClick, CloseOnDimmerClick(..)
  , pattern DefaultOpen, DefaultOpen(..)
  , pattern DimmerType, DimmerType(..)
  , pattern Scrollable, Scrollable(..)
  , pattern Size, Size(..)
  , pattern WithPortal, WithPortal(..)
  , pattern IsImage, IsImage(..)
  , pattern Scrolling, Scrolling(..)
  )

import Data.Function as Tools ((&))

data Modal = Modal_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , basic :: Bool
    , closeOnDimmerClick :: Bool
    , closeOnDocumentClick :: Bool
    , defaultOpen :: Bool
    , dimmer :: Maybe Txt
    , mountNode :: Maybe Element
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
        { as = \fs cs -> Div & Features fs & Children cs
        , dimmer = Just ""
        , closeOnDimmerClick = True
        , closeOnDocumentClick = True
        , withPortal = id
        }

pattern Modal :: Modal -> Modal
pattern Modal m = m

data ModalState = MS
    { topMargin :: Maybe Int
    , scrolling :: Maybe Bool
    , active :: Bool
    , ref :: IORef (Maybe JSV)
    , pendingAnimation :: IORef (IO ())
    }

instance Pure Modal where
    view =
        LibraryComponentIO $ \self ->
            let
                getMountNode = do
                    Modal_ {..} <- ask self
                    b <- getBody
                    return $ fromMaybe (Element $ toJSV b) mountNode

                handleRef (Node n) = do
                    MS {..} <- get self
                    writeIORef ref (Just n)

                handleOpen _ = do
                    Modal_ {..} <- ask self
                    onOpen
                    modify_ self $ \_ MS {..} -> MS { active = True, .. }

                handleClose = do
                    Modal_ {..} <- ask self
                    onClose
                    modify_ self $ \_ MS {..} -> MS { active = False, .. }

                handlePortalMount = do
                    Modal_ {..} <- ask self
                    modify_ self $ \_ MS {..} -> MS { scrolling = Just False, .. }
                    setPositionAndClassNames
                    onMount

                handlePortalUnmount = do
                    Modal_ {..} <- ask self
                    MS     {..} <- get self
                    n <- toJSV <$> getMountNode
                    traverse_ (removeClass n) ["blurring","dimmable","dimmed","scrolling"]
                    writeIORef pendingAnimation def
                    onUnmount

                setPositionAndClassNames = do
                    Modal_ {..} <- ask self
                    MS     {..} <- get self
                    n           <- toJSV <$> getMountNode

                    when (isJust dimmer) $
                      traverse_ (addClass n) ["dimmable","dimmed"]
                    when (dimmer == Just "blurring") $ do
                      addClass n "blurring"

                    mr <- readIORef ref

                    for_ mr $ \r -> do
                      BR { brHeight = h } <- boundingRect (Element r)

                      ih <- innerHeight

                      let topMargin' = negate (round (h / 2))
                          scrolling' = h >= fromIntegral ih

                          scrollingChange = scrolling /= Just scrolling'
                          topMarginChange = topMargin /= Just topMargin'

                      when scrollingChange $
                        (scrolling' ? addClass n $ removeClass n)
                          "scrolling"

                      (scrollingChange || topMarginChange) #
                        modify_ self (\_ MS {..} ->
                          MS { topMargin = Just topMargin'
                             , scrolling = Just scrolling'
                             , ..
                             })

                    writeIORef pendingAnimation setPositionAndClassNames
                    void $ addAnimation (join $ readIORef pendingAnimation)

            in def
                { construct = do
                    Modal_ {..} <- ask self
                    MS def def (open || defaultOpen) <$> newIORef def <*> newIORef def
                , receive = \newprops oldstate -> return $
                    (open newprops /= active oldstate)
                      ? oldstate { active = open newprops }
                      $ oldstate
                , unmounted = do
                    Modal_ {..} <- ask self
                    handlePortalUnmount
                , render = \Modal_ {..} MS {..} ->
                    let
                        dimmerClasses
                          | isJust dimmer =
                                [ "ui"
                                , (dimmer == Just "inverted") # "inverted"
                                , "page modals dimmer transition visible active"
                                ]
                          | otherwise = []

                        viewContent f =
                            let
                                ss = maybe [] (\tm -> [(marginTop,pxs tm)]) topMargin

                                cs =
                                    [ "ui"
                                    , size
                                    , basic # "basic"
                                    , (scrolling == Just True) # "scrolling"
                                    , "modal transition visible active"
                                    ]

                            in
                                as (f $ features & Classes cs & Styles ss & Lifecycle (HostRef handleRef)) children

                    in (View :: Portal.Portal -> View) $ Portal.Portal $ withPortal $ def
                        & (closeOnDocumentClick ? CloseOnDocumentClick True $ id)
                        & (closeOnDimmerClick   ? CloseOnRootNodeClick True $ id)
                        & Portal.PortalNode viewContent
                        & MountNode mountNode
                        & Classes dimmerClasses
                        & Open active
                        & Portal.OnClose handleClose
                        & OnMount handlePortalMount
                        & OnOpen handleOpen
                        & OnUnmounted handlePortalUnmount
                        & Children [ trigger ]
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
    type Prop MountNode Modal = Maybe Element
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

instance HasProp OnUnmounted Modal where
    type Prop OnUnmounted Modal = IO ()
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
    view Actions_ {..} = as (features & Class "actions") children

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
    view Content_ {..} =
        let
            cs =
                [ image # "image"
                , scrolling # "scrolling"
                , "content"
                ]

        in
            as (features & Classes cs) children

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
    view Description_ {..} = as (features & Class "description") children

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
    view Header_ {..} = as (features & Class "header") children

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

