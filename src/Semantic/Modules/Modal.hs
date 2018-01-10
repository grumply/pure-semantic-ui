{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Modal (module Semantic.Modules.Modal, module Export) where

import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (round,addClass,trigger,OnClose)
import Pure.DOM (addAnimation)
import Pure.Lifted (body,IsJSV(..),JSV,Node(..),Element(..))

import Semantic.Utils

import qualified Semantic.Addons.Portal as Portal

import Semantic.Properties.CloseOnDocumentClick
import Semantic.Properties.CloseOnRootNodeClick
import Semantic.Properties.OpenOnTriggerClick
import Semantic.Properties.OnClose
import Semantic.Properties.OnMount
import Semantic.Properties.OnOpen
import Semantic.Properties.OnUnmount
import Semantic.Properties.Open
import Semantic.Properties.Trigger
import Semantic.Properties.MountNode

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Basic
import Semantic.Properties.CloseOnDimmerClick
import Semantic.Properties.DefaultOpen
import Semantic.Properties.DimmerType
import Semantic.Properties.Size
import Semantic.Properties.Styles
import Semantic.Properties.WithPortal

import Semantic.Modules.Modal.ModalActions as Export
import Semantic.Modules.Modal.ModalContent as Export
import Semantic.Modules.Modal.ModalDescription as Export
import Semantic.Modules.Modal.ModalHeader as Export

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
    , size :: Txt
    , styles :: [(Txt,Txt)]
    , withPortal :: Portal.Portal ms -> Portal.Portal ms
    , trigger :: View ms
    } deriving (Generic)

instance Default (Modal ms) where
    def = (G.to gdef) 
        { as = Div 
        , closeOnDimmerClick = True
        , closeOnDocumentClick = True
        }

pattern Modal :: VC ms => Modal ms -> View ms
pattern Modal m = View m

data ModalState = MS
    { topMargin :: Maybe Int
    , scrolling :: Maybe Bool
    , open :: Bool
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
                        MS { open = False, .. }

                handleOpen _ = do
                    Modal_ {..} <- getProps self
                    onOpen
                    void $ setState self $ \_ MS {..} ->
                        MS { open = True, .. }

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

                setPositionAndClassNames :: IO ()
                setPositionAndClassNames = do
                    Modal_ {..} <- getProps self
                    MS     {..} <- getState self
                    n           <- getMountNode

                    dimmer # traverse_ (addClass n) ["dimmable","dimmed"]
                    (dimmer == Just "blurring") # addClass n "blurring"

                    mr <- readIORef ref
                    for_ mr $ \r -> do
                        (_,h,_,_) <- boundingRect (Element r)
                        ih <- innerHeight

                        let topMargin' = negate (round (h / 2))
                            scrolling' = h >= fromIntegral ih

                        (scrolling /= Just scrolling') #
                            (scrolling' ? addClass n $ removeClass n) 
                                "scrolling"

                        (topMargin /= Just topMargin' || scrolling /= Just scrolling') #
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
                    MS def def defaultOpen <$> newIORef def <*> newIORef def
                , unmount = do
                    Modal_ {..} <- getProps self
                    handlePortalUnmount
                    parent self onUnmount
                , renderer = \Modal_ {..} MS { open = o, ..} -> 
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
                        & Open o
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