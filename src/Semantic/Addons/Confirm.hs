{-# LANGUAGE UndecidableInstances #-}
module Semantic.Addons.Confirm where

import GHC.Generics as G
import Pure.View hiding (content,Button,Size,OnClose,OnClick)

import Semantic.Utils

import Semantic.Elements.Button as Button

import Semantic.Modules.Modal

import Semantic.Properties.OnClick
import Semantic.Properties.OnClose
import Semantic.Properties.Primary
import Semantic.Properties.Size

import Semantic.Properties.Children

data Confirm ms = Confirm_
    { cancelButton :: Button ms
    , confirmButton :: Button ms
    , header :: ModalHeader ms
    , content :: ModalContent ms
    , onCancel :: Ef ms IO ()
    , onConfirm :: Ef ms IO ()
    , open :: Bool
    , withModal :: Modal ms -> Modal ms
    } deriving (Generic)

instance Default (Confirm ms) where
    def = (G.to gdef) 
        { cancelButton  = def & Children [ "Cancel" ]
        , confirmButton = def & Children [ "OK" ]
        , content       = def & Children [ "Are you sure?" ]
        }
    
pattern Confirm :: VC ms => Confirm ms -> View ms
pattern Confirm c = View c

instance VC ms => Pure Confirm ms where
    render Confirm_ {..} =
        let handleCancel = do
                Button.onClick cancelButton
                onCancel
            handleConfirm = do
                Button.onClick confirmButton
                onConfirm
        in Modal $ withModal $ def & Size "small" & OnClose onCancel & Children 
            [ ModalHeader header 
            , ModalContent content
            , ModalActions def & Children
                [ Button $ cancelButton & OnClick handleCancel
                , Button $ confirmButton & Primary & OnClick handleConfirm 
                ]
            ]
