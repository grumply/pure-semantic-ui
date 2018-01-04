module Semantic.Modules.Modal.ModalHeader where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

data ModalHeader ms = ModalHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ModalHeader ms) where
    def = (G.to gdef) { as = Div }

pattern ModalHeader :: Typeable ms => ModalHeader ms -> View ms
pattern ModalHeader mh = View mh

instance Typeable ms => Pure ModalHeader ms where
    render ModalHeader_ {..} =
        let
            cs = classes <> [ "header" ]

        in
            as
                ( ClassList cs
                : attributes
                ) 
                children
