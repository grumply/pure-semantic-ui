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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

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
  , pattern Classes, Classes(..)
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

data Modal ms = Modal_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , basic :: Bool
    , closeOnDimmerClick :: Bool
    , closeOnDocumentClick :: Bool
    , defaultOpen :: Bool
    , dimmer :: Maybe Txt
    , mountNode :: Maybe JSV
    , onClose :: Ef ms IO ()
    , onMount :: Ef ms IO ()
    , onOpen :: Ef ms IO ()
    , onUnmount :: Ef ms IO ()
    , open :: Bool
    , scrollable :: Bool
    , size :: Txt
    , styles :: [(Txt,Txt)]
    , withPortal :: Portal.Portal ms -> Portal.Portal ms
    , trigger :: View ms
    } deriving (Generic)

instance Default (Modal ms) where
    def = (G.to gdef)
        { as = Div
        , dimmer = Just ""
        , closeOnDimmerClick = True
        , closeOnDocumentClick = True
        , withPortal = id
        }

pattern Modal :: VC ms => Modal ms -> View ms
pattern Modal m = View m

data ModalState = MS
    { topMargin :: Maybe Int
    , scrolling :: Maybe Bool
    , active :: Bool
    , ref :: IORef (Maybe JSV)
    , pendingAnimation :: IORef (IO ())
    }

instance VC ms => Pure Modal ms where
    render m =
        Component "Semantic.Modules.Modal" m $ \self ->
            let
                getMountNode = do
                    Modal_ {..} <- getProps self
                    return $ fromMaybe (toJSV body) mountNode

                handleRef (Node n) = do
                    MS {..} <- getState self
                    writeIORef ref (Just n)
                    return Nothing

                handleClose = do
                    Modal_ {..} <- getProps self
                    onClose
                    void $ setState self $ \_ MS {..} ->
                        MS { active = False, .. }

                handleOpen _ = do
                    Modal_ {..} <- getProps self
                    onOpen
                    void $ setState self $ \_ MS {..} ->
                        MS { active = True, .. }

                handlePortalMount = do
                    Modal_ {..} <- getProps self
                    setState self $ \_ MS {..} ->
                        MS { scrolling = Just False, .. }
                    liftIO setPositionAndClassNames
                    onMount

                handlePortalUnmount = do
                    Modal_ {..} <- getProps self
                    MS     {..} <- getState self
                    n <- getMountNode
                    traverse_ (removeClass n) ["blurring","dimmable","dimmed","scrolling"]
                    writeIORef pendingAnimation def

                setPositionAndClassNames = do
                    Modal_ {..} <- getProps self
                    MS     {..} <- getState self
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
                        setState self $ \_ MS {..} ->
                          MS { topMargin = Just topMargin'
                             , scrolling = Just scrolling'
                             , ..
                             }

                    writeIORef pendingAnimation setPositionAndClassNames
                    void $ addAnimation (join $ readIORef pendingAnimation)

            in def
                { construct = do
                    Modal_ {..} <- getProps self
                    MS def def (open || defaultOpen) <$> newIORef def <*> newIORef def
                , receiveProps = \newprops oldstate -> return $
                    (open newprops /= active oldstate)
                      ? oldstate { active = open newprops }
                      $ oldstate
                , unmount = do
                    Modal_ {..} <- getProps self
                    handlePortalUnmount
                    parent self onUnmount
                , renderer = \Modal_ {..} MS {..} ->
                    let
                        dimmerClasses =
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
                                    : classes
                                    )

                                children' = flip map children $ \c ->
                                    case c of
                                        -- add ModalAction mapping here
                                        _ -> c

                            in
                                as
                                    ( mergeClasses $ ClassList cs
                                    : StyleList ss
                                    : HostRef handleRef
                                    : attributes
                                    )
                                    children

                    in Portal.Portal $ withPortal $ def
                        & (closeOnDocumentClick ? CloseOnDocumentClick True $ id)
                        & (closeOnDimmerClick   ? CloseOnRootNodeClick True $ id)
                        & Trigger trigger
                        & Classes dimmerClasses
                        & MountNode mountNode
                        & Open active
                        & OnClose handleClose
                        & OnMount handlePortalMount
                        & OnOpen handleOpen
                        & OnUnmount (liftIO handlePortalUnmount >> onUnmount)
                        & Children [ renderContent ]
                }

instance HasProp As (Modal ms) where
    type Prop As (Modal ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a m = m { as = a }

instance HasProp Attributes (Modal ms) where
    type Prop Attributes (Modal ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as m = m { attributes = as }

instance HasProp Children (Modal ms) where
    type Prop Children (Modal ms) = [View ms]
    getProp _ = children
    setProp _ cs m = m { children = cs }

instance HasProp Classes (Modal ms) where
    type Prop Classes (Modal ms) = [Txt]
    getProp _ = classes
    setProp _ cs m = m { classes = cs }

instance HasProp Basic (Modal ms) where
    type Prop Basic (Modal ms) = Bool
    getProp _ = basic
    setProp _ b m = m { basic = b }

instance HasProp CloseOnDimmerClick (Modal ms) where
    type Prop CloseOnDimmerClick (Modal ms) = Bool
    getProp _ = closeOnDimmerClick
    setProp _ codc m = m { closeOnDimmerClick = codc }

instance HasProp CloseOnDocumentClick (Modal ms) where
    type Prop CloseOnDocumentClick (Modal ms) = Bool
    getProp _ = closeOnDocumentClick
    setProp _ codc m = m { closeOnDocumentClick = codc }

instance HasProp DefaultOpen (Modal ms) where
    type Prop DefaultOpen (Modal ms) = Bool
    getProp _ = defaultOpen
    setProp _ o m = m { defaultOpen = o }

instance HasProp DimmerType (Modal ms) where
    type Prop DimmerType (Modal ms) = Maybe Txt
    getProp _ = dimmer
    setProp _ d m = m { dimmer = d }

instance HasProp MountNode (Modal ms) where
    type Prop MountNode (Modal ms) = Maybe JSV
    getProp _ = mountNode
    setProp _ mn m = m { mountNode = mn }

instance HasProp OnClose (Modal ms) where
    type Prop OnClose (Modal ms) = Ef ms IO ()
    getProp _ = onClose
    setProp _ oc m = m { onClose = oc }

instance HasProp OnMount (Modal ms) where
    type Prop OnMount (Modal ms) = Ef ms IO ()
    getProp _ = onMount
    setProp _ om m = m { onMount = om }

instance HasProp OnOpen (Modal ms) where
    type Prop OnOpen (Modal ms) = Ef ms IO ()
    getProp _ = onOpen
    setProp _ oo m = m { onOpen = oo }

instance HasProp OnUnmount (Modal ms) where
    type Prop OnUnmount (Modal ms) = Ef ms IO ()
    getProp _ = onUnmount
    setProp _ ou m = m { onUnmount = ou }

instance HasProp Open (Modal ms) where
    type Prop Open (Modal ms) = Bool
    getProp _ = open
    setProp _ o m = m { open = o }

instance HasProp Scrollable (Modal ms) where
    type Prop Scrollable (Modal ms) = Bool
    getProp _ = scrollable
    setProp _ s m = m { scrollable = s }

instance HasProp Size (Modal ms) where
    type Prop Size (Modal ms) = Txt
    getProp _ = size
    setProp _ s m = m { size = s }

instance HasProp Styles (Modal ms) where
    type Prop Styles (Modal ms) = [(Txt,Txt)]
    getProp _ = styles
    setProp _ ss m = m { styles = ss }

instance HasProp WithPortal (Modal ms) where
    type Prop WithPortal (Modal ms) = Portal.Portal ms -> Portal.Portal ms
    getProp _ = withPortal
    setProp _ wp m = m { withPortal = wp }

instance HasProp Trigger (Modal ms) where
    type Prop Trigger (Modal ms) = View ms
    getProp _ = trigger
    setProp _ t m = m { trigger = t }

data Actions ms = Actions_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Actions ms) where
    def = (G.to gdef) { as = Div }

pattern Actions :: Actions ms -> View ms
pattern Actions ma = View ma

instance Pure Actions ms where
    render Actions_ {..} =
        as
            ( ClassList ( "actions" : classes )
            : attributes
            )
            children

instance HasProp As (Actions ms) where
    type Prop As (Actions ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f ma = ma { as = f }

instance HasProp Attributes (Actions ms) where
    type Prop Attributes (Actions ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs ma = ma { attributes = cs }

instance HasProp Children (Actions ms) where
    type Prop Children (Actions ms) = [View ms]
    getProp _ = children
    setProp _ cs ma = ma { children = cs }

instance HasProp Classes (Actions ms) where
    type Prop Classes (Actions ms) = [Txt]
    getProp _ = classes
    setProp _ cs ma = ma { classes = cs }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , image :: Bool
    , scrolling :: Bool
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content mc = View mc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs = classes ++
                [ image # "image"
                , scrolling # "scrolling"
                , "content"
                ]

        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Content ms) where
    type Prop As (Content ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f mc = mc { as = f }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs mc = mc { attributes = cs }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs mc = mc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs mc = mc { classes = cs }

instance HasProp IsImage (Content ms) where
    type Prop IsImage (Content ms) = Bool
    getProp _ = image
    setProp _ ii mc = mc { image = ii }

instance HasProp Scrolling (Content ms) where
    type Prop Scrolling (Content ms) = Bool
    getProp _ = scrolling
    setProp _ s mc = mc { scrolling = s }

data Description ms = Description_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Description ms) where
    def = (G.to gdef) { as = Div }

pattern Description :: Description ms -> View ms
pattern Description md = View md

instance Pure Description ms where
    render Description_ {..} =
        let
            cs =
                ( "description"
                : classes
                )

        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Description ms) where
    type Prop As (Description ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f md = md { as = f }

instance HasProp Attributes (Description ms) where
    type Prop Attributes (Description ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs md = md { attributes = cs }

instance HasProp Children (Description ms) where
    type Prop Children (Description ms) = [View ms]
    getProp _ = children
    setProp _ cs md = md { children = cs }

instance HasProp Classes (Description ms) where
    type Prop Classes (Description ms) = [Txt]
    getProp _ = classes
    setProp _ cs md = md { classes = cs }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header mh = View mh

instance Pure Header ms where
    render Header_ {..} =
        let
            cs = classes <> [ "header" ]

        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Header ms) where
    type Prop As (Header ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f mh = mh { as = f }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs mh = mh { attributes = cs }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs mh = mh { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs mh = mh { classes = cs }
