{-# LANGUAGE UndecidableInstances #-}
module Semantic.Confirm
  ( module Properties
  , module Tools
  , module Button
  , module Modal
  , Confirm(..), pattern Confirm
  ) where

import Pure hiding (Size,content)

import Control.Arrow ((&&&))
import GHC.Generics as G

import Semantic.Utils

import qualified Semantic.Button as Button

import qualified Semantic.Modal as Modal

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Primary, Primary(..)
  , pattern Size, Size(..)
  , pattern Open, Open(..)
  , pattern OnCancel, OnCancel(..)
  , pattern OnConfirm, OnConfirm(..)
  , pattern CancelButton, CancelButton(..)
  , pattern ConfirmButton, ConfirmButton(..)
  , pattern WithModal, WithModal(..)
  )

import Data.Function as Tools ((&))

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

pattern Confirm :: Confirm -> Confirm
pattern Confirm c = c

instance Pure Confirm where
    view Confirm_ {..} =
      View $ Modal.Modal $ withModal $ def & Size "small" & OnClose (\_ -> onCancel) & Children
        [ View $ Modal.Header header
        , View $ Modal.Content content
        , View $ Modal.Actions $ def & Children
            [ View $ Button.Button $ cancelButton & OnClick (\_ -> onCancel)
            , View $ Button.Button $ confirmButton & Primary True & OnClick (\_ -> onConfirm)
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
pattern ConfirmContent mc c <- (Semantic.Confirm.content &&& id -> (mc,c)) where
    ConfirmContent mc c = c { content = mc }

instance HasProp WithModal Confirm where
    type Prop WithModal Confirm = Modal.Modal -> Modal.Modal
    getProp _ = withModal
    setProp _ wm c = c { withModal = wm }
