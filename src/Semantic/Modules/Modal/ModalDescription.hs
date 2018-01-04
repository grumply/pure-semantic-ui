module Semantic.Modules.Modal.ModalDescription where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

data ModalDescription ms = ModalDescription_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ModalDescription ms) where
    def = (G.to gdef) { as = Div }

pattern ModalDescription :: Typeable ms => ModalDescription ms -> View ms
pattern ModalDescription md = View md

instance Typeable ms => Pure ModalDescription ms where
    render ModalDescription_ {..} =
        let
            cs =
                ( "description"
                : classes
                )

        in
            as
                ( ClassList cs
                : attributes
                ) 
                children
