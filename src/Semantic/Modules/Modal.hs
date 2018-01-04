{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Modal where

import Data.IORef
import Data.Maybe
import GHC.Generics as G
import Pure.View hiding (round,addClass,trigger,OnClose)
import Pure.DOM (addAnimation)
import Pure.Lifted (JSV,Node(..),Element(..))

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

import Semantic.Properties.Children
import Semantic.Properties.Classes
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
    , onActionClick :: Ef ms IO ()
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
                    return $ fromMaybe body mountNode 

                handleRef (Node n) = do
                    MS {..} <- getState self
                    writeIORef ref (Just n)
                    return Nothing

                handleClose = do
                    Modal_ {..} <- getProps self
                    onClose
                    void $ setState self $ \_ MS {..} -> 
                        MS { open = False, .. }

                handleOpen = do
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
                                    ( ClassList cs
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
