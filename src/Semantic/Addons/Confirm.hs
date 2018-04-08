{-# LANGUAGE UndecidableInstances #-}
module Semantic.Addons.Confirm
  ( module Properties
  , module Tools
  , module Button
  , module Modal
  , Confirm(..), pattern Confirm
  ) where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (content,Button,Size,OnClose,OnClick)

import Semantic.Utils

import Semantic.Elements.Button as Button

import Semantic.Modules.Modal as Modal

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasOnClickProp(..)       , pattern OnClick
  , HasOnCloseProp(..)       , pattern OnClose
  , HasPrimaryProp(..)       , pattern Primary
  , HasSizeProp(..)          , pattern Size
  , HasChildrenProp(..)      , pattern Children
  , HasOpenProp(..)          , pattern Open
  , HasOnCancelProp(..)      , pattern OnCancel
  , HasOnConfirmProp(..)     , pattern OnConfirm
  , HasCancelButtonProp(..)  , pattern CancelButton
  , HasConfirmButtonProp(..) , pattern ConfirmButton
  , HasWithModalProp(..)     , pattern WithModal
  )

data Confirm ms = Confirm_
    { cancelButton  :: Button ms
    , confirmButton :: Button ms
    , header        :: ModalHeader ms
    , content       :: ModalContent ms
    , onCancel      :: Ef ms IO ()
    , onConfirm     :: Ef ms IO ()
    , open          :: Bool
    , withModal     :: Modal ms -> Modal ms
    } deriving (Generic)

instance Default (Confirm ms) where
    def = (G.to gdef)
        { cancelButton  = def & Children [ "Cancel" ]
        , confirmButton = def & Children [ "OK" ]
        , content       = def & Children [ "Are you sure?" ]
        , withModal     = id
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
            , ModalActions $ def & Children
                [ Button $ cancelButton & OnClick handleCancel
                , Button $ confirmButton & Primary & OnClick handleConfirm
                ]
            ]

instance HasOpenProp (Confirm ms) where
    getOpen = open
    setOpen o c = c { open = o }

instance HasCancelButtonProp (Confirm ms) where
    type CancelButtonProp (Confirm ms) = Button ms
    getCancelButton = cancelButton
    setCancelButton cb c = c { cancelButton = cb }

instance HasConfirmButtonProp (Confirm ms) where
    type ConfirmButtonProp (Confirm ms) = Button ms
    getConfirmButton = confirmButton
    setConfirmButton cb c = c { confirmButton = cb }

pattern ConfirmHeader :: ModalHeader ms -> Confirm ms -> Confirm ms
pattern ConfirmHeader mh c <- (header &&& id -> (mh,c)) where
    ConfirmHeader mh c = c { header = mh }

pattern ConfirmContent :: ModalContent ms -> Confirm ms -> Confirm ms
pattern ConfirmContent mc c <- (content &&& id -> (mc,c)) where
    ConfirmContent mc c = c { content = mc }

instance HasOnCancelProp (Confirm ms) where
    type OnCancelProp (Confirm ms) = Ef ms IO ()
    getOnCancel = onCancel
    setOnCancel oc c = c { onCancel = oc }

instance HasOnConfirmProp (Confirm ms) where
    type OnConfirmProp (Confirm ms) = Ef ms IO ()
    getOnConfirm = onConfirm
    setOnConfirm oc c = c { onConfirm = oc }

instance HasWithModalProp (Confirm ms) where
    type WithModalProp (Confirm ms) = Modal ms -> Modal ms
    getWithModal = withModal
    setWithModal wm c = c { withModal = wm }
