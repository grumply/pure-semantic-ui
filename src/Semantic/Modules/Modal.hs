{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Modal where

import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (active,round,addClass,trigger,OnClose)
import Pure.DOM (addAnimation)
import Pure.Lifted (body,IsJSV(..),JSV,Node(..),Element(..))

import Semantic.Utils

import qualified Semantic.Addons.Portal as Portal

import Semantic.Properties as Properties
  ( HasCloseOnDocumentClickProp(..), pattern CloseOnDocumentClick
  , HasCloseOnRootNodeClickProp(..), pattern CloseOnRootNodeClick
  , HasOpenOnTriggerClickProp(..), pattern OpenOnTriggerClick
  , HasOnCloseProp(..), pattern OnClose
  , HasOnMountProp(..), pattern OnMount
  , HasOnOpenProp(..), pattern OnOpen
  , HasOnUnmountProp(..), pattern OnUnmount
  , HasOpenProp(..), pattern Open
  , HasTriggerProp(..), pattern Trigger
  , HasMountNodeProp(..), pattern MountNode
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasBasicProp(..), pattern Basic
  , HasCloseOnDimmerClickProp(..), pattern CloseOnDimmerClick
  , HasDefaultOpenProp(..), pattern DefaultOpen
  , HasDimmerTypeProp(..), pattern DimmerType
  , HasScrollableProp(..), pattern Scrollable
  , HasSizeProp(..), pattern Size
  , HasStylesProp(..), pattern Styles
  , HasWithPortalProp(..), pattern WithPortal
  , HasIsImageProp(..), pattern IsImage
  , HasScrollingProp(..), pattern Scrolling
  )

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
                        & (closeOnDocumentClick ? CloseOnDocumentClick $ id)
                        & (closeOnDimmerClick   ? CloseOnRootNodeClick $ id)
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

instance HasAsProp (Modal ms) where
    type AsProp (Modal ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a m = m { as = a }

instance HasAttributesProp (Modal ms) where
    type Attribute (Modal ms) = Feature ms
    getAttributes = attributes
    setAttributes as m = m { attributes = as }

instance HasChildrenProp (Modal ms) where
    type Child (Modal ms) = View ms
    getChildren = children
    setChildren cs m = m { children = cs }

instance HasClassesProp (Modal ms) where
    getClasses = classes
    setClasses cs m = m { classes = cs }

instance HasBasicProp (Modal ms) where
    type BasicProp (Modal ms) = Bool
    getBasic = basic
    setBasic b m = m { basic = b }

instance HasCloseOnDimmerClickProp (Modal ms) where
    getCloseOnDimmerClick = closeOnDimmerClick
    setCloseOnDimmerClick codc m = m { closeOnDimmerClick = codc }

instance HasCloseOnDocumentClickProp (Modal ms) where
    getCloseOnDocumentClick = closeOnDocumentClick
    setCloseOnDocumentClick codc m = m { closeOnDocumentClick = codc }

instance HasDefaultOpenProp (Modal ms) where
    getDefaultOpen = defaultOpen
    setDefaultOpen o m = m { defaultOpen = o }

instance HasDimmerTypeProp (Modal ms) where
    getDimmerType = dimmer
    setDimmerType d m = m { dimmer = d }

instance HasMountNodeProp (Modal ms) where
    getMountNode = mountNode
    setMountNode mn m = m { mountNode = mn }

instance HasOnCloseProp (Modal ms) where
    type OnCloseProp (Modal ms) = Ef ms IO ()
    getOnClose = onClose
    setOnClose oc m = m { onClose = oc }

instance HasOnMountProp (Modal ms) where
    type OnMountProp (Modal ms) = Ef ms IO ()
    getOnMount = onMount
    setOnMount om m = m { onMount = om }

instance HasOnOpenProp (Modal ms) where
    type OnOpenProp (Modal ms) = Ef ms IO ()
    getOnOpen = onOpen
    setOnOpen oo m = m { onOpen = oo }

instance HasOnUnmountProp (Modal ms) where
    type OnUnmountProp (Modal ms) = Ef ms IO ()
    getOnUnmount = onUnmount
    setOnUnmount ou m = m { onUnmount = ou }

instance HasOpenProp (Modal ms) where
    getOpen = open
    setOpen o m = m { open = o }

instance HasScrollableProp (Modal ms) where
    getScrollable = scrollable
    setScrollable s m = m { scrollable = s }

instance HasSizeProp (Modal ms) where
    getSize = size
    setSize s m = m { size = s }

instance HasStylesProp (Modal ms) where
    getStyles = styles
    setStyles ss m = m { styles = ss }

instance HasWithPortalProp (Modal ms) where
    type WithPortalProp (Modal ms) = Portal.Portal ms -> Portal.Portal ms
    getWithPortal = withPortal
    setWithPortal wp m = m { withPortal = wp }

instance HasTriggerProp (Modal ms) where
    type TriggerProp (Modal ms) = View ms
    getTrigger = trigger
    setTrigger t m = m { trigger = t }

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

instance HasAsProp (Actions ms) where
    type AsProp (Actions ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ma = ma { as = f }

instance HasAttributesProp (Actions ms) where
    type Attribute (Actions ms) = Feature ms
    getAttributes = attributes
    setAttributes cs ma = ma { attributes = cs }

instance HasChildrenProp (Actions ms) where
    type Child (Actions ms) = View ms
    getChildren = children
    setChildren cs ma = ma { children = cs }

instance HasClassesProp (Actions ms) where
    getClasses = classes
    setClasses cs ma = ma { classes = cs }

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

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f mc = mc { as = f }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes cs mc = mc { attributes = cs }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs mc = mc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs mc = mc { classes = cs }

instance HasIsImageProp (Content ms) where
    getIsImage = image
    setIsImage ii mc = mc { image = ii }

instance HasScrollingProp (Content ms) where
    getScrolling = scrolling
    setScrolling s mc = mc { scrolling = s }

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

instance HasAsProp (Description ms) where
    type AsProp (Description ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f md = md { as = f }

instance HasAttributesProp (Description ms) where
    type Attribute (Description ms) = Feature ms
    getAttributes = attributes
    setAttributes cs md = md { attributes = cs }

instance HasChildrenProp (Description ms) where
    type Child (Description ms) = View ms
    getChildren = children
    setChildren cs md = md { children = cs }

instance HasClassesProp (Description ms) where
    getClasses = classes
    setClasses cs md = md { classes = cs }

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

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f mh = mh { as = f }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes cs mh = mh { attributes = cs }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs mh = mh { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs mh = mh { classes = cs }
