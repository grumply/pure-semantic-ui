module Semantic.Modules.Modal.ModalActions where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

data ModalActions ms = ModalActions_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (ModalActions ms) where
    def = (G.to gdef) { as = Div }

pattern ModalActions :: Typeable ms => ModalActions ms -> View ms
pattern ModalActions ma = View ma

instance Typeable ms => Pure ModalActions ms where
    render ModalActions_ {..} =
        as
            ( ClassList ( "actions" : classes )
            : attributes
            ) 
            children
