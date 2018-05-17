{-# LANGUAGE UndecidableInstances #-}
module Semantic.Confirm
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

import qualified Semantic.Button as Button

import qualified Semantic.Modal as Modal

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Primary, Primary(..)
  , pattern Size, Size(..)
  , pattern Children, Children(..)
  , pattern Open, Open(..)
  , pattern OnCancel, OnCancel(..)
  , pattern OnConfirm, OnConfirm(..)
  , pattern CancelButton, CancelButton(..)
  , pattern ConfirmButton, ConfirmButton(..)
  , pattern WithModal, WithModal(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

-- TODO: remove specialized children
data Confirm = Confirm_
    { cancelButton  :: Button.Button
    , confirmButton :: Button.Button
    , header        :: Modal.Header
    , content       :: Modal.Content
    , onCancel      :: IO ()
    , onConfirm     :: IO ()
    , open          :: Bool
    , withModal     :: Modal.Modal -> Modal.Modal
    } deriving (Generic)

instance Default Confirm where
    def = (G.to gdef)
        { cancelButton  = def & Children [ "Cancel" ]
        , confirmButton = def & Children [ "OK" ]
        , content       = def & Children [ "Are you sure?" ]
        , withModal     = id
        }

pattern Confirm :: Confirm -> VC
pattern Confirm c = c

instance VC => Pure Confirm where
    render Confirm_ {..} =
        let handleCancel = do
                Button.onClick cancelButton
                onCancel
            handleConfirm = do
                Button.onClick confirmButton
                onConfirm
        in Modal.Modal $ withModal $ def & Size "small" & OnClose onCancel & Children
            [ Modal.Header header
            , Modal.Content content
            , Modal.Actions $ def & Children
                [ Button.Button $ cancelButton & OnClick handleCancel
                , Button.Button $ confirmButton & Primary True & OnClick handleConfirm
                ]
            ]

instance HasProp Open Confirm where
    type Prop Open Confirm = Bool
    getProp _ = open
    setProp _ o c = c { open = o }

instance HasProp CancelButton Confirm where
    type Prop CancelButton Confirm = Button.Button
    getProp _ = cancelButton
    setProp _ cb c = c { cancelButton = cb }

instance HasProp ConfirmButton Confirm where
    type Prop ConfirmButton Confirm = Button.Button
    getProp _ = confirmButton
    setProp _ cb c = c { confirmButton = cb }

pattern ConfirmHeader :: Modal.Header -> Confirm -> Confirm
pattern ConfirmHeader mh c <- (header &&& id -> (mh,c)) where
    ConfirmHeader mh c = c { header = mh }

pattern ConfirmContent :: Modal.Content -> Confirm -> Confirm
pattern ConfirmContent mc c <- (content &&& id -> (mc,c)) where
    ConfirmContent mc c = c { content = mc }

instance HasProp WithModal Confirm where
    type Prop WithModal Confirm = Modal.Modal -> Modal.Modal
    getProp _ = withModal
    setProp _ wm c = c { withModal = wm }
