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

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!) )

import Semantic.Properties as Properties
  ( pattern OnClick, OnClick(..)
  , pattern OnClose, OnClose(..)
  , pattern Primary, Primary(..)
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

data Confirm ms = Confirm_
    { cancelButton  :: Button.Button ms
    , confirmButton :: Button.Button ms
    , header        :: Modal.Header ms
    , content       :: Modal.Content ms
    , onCancel      :: Ef ms IO ()
    , onConfirm     :: Ef ms IO ()
    , open          :: Bool
    , withModal     :: Modal.Modal ms -> Modal.Modal ms
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
        in Modal.Modal $ withModal $ def & Size "small" & OnClose onCancel & Children
            [ Modal.Header header
            , Modal.Content content
            , Modal.Actions $ def & Children
                [ Button.Button $ cancelButton & OnClick handleCancel
                , Button.Button $ confirmButton & Primary True & OnClick handleConfirm
                ]
            ]

instance HasProp Open (Confirm ms) where
    type Prop Open (Confirm ms) = Bool
    getProp _ = open
    setProp _ o c = c { open = o }

instance HasProp CancelButton (Confirm ms) where
    type Prop CancelButton (Confirm ms) = Button.Button ms
    getProp _ = cancelButton
    setProp _ cb c = c { cancelButton = cb }

instance HasProp ConfirmButton (Confirm ms) where
    type Prop ConfirmButton (Confirm ms) = Button.Button ms
    getProp _ = confirmButton
    setProp _ cb c = c { confirmButton = cb }

pattern ConfirmHeader :: Modal.Header ms -> Confirm ms -> Confirm ms
pattern ConfirmHeader mh c <- (header &&& id -> (mh,c)) where
    ConfirmHeader mh c = c { header = mh }

pattern ConfirmContent :: Modal.Content ms -> Confirm ms -> Confirm ms
pattern ConfirmContent mc c <- (content &&& id -> (mc,c)) where
    ConfirmContent mc c = c { content = mc }

instance HasProp OnCancel (Confirm ms) where
    type Prop OnCancel (Confirm ms) = Ef ms IO ()
    getProp _ = onCancel
    setProp _ oc c = c { onCancel = oc }

instance HasProp OnConfirm (Confirm ms) where
    type Prop OnConfirm (Confirm ms) = Ef ms IO ()
    getProp _ = onConfirm
    setProp _ oc c = c { onConfirm = oc }

instance HasProp WithModal (Confirm ms) where
    type Prop WithModal (Confirm ms) = Modal.Modal ms -> Modal.Modal ms
    getProp _ = withModal
    setProp _ wm c = c { withModal = wm }
