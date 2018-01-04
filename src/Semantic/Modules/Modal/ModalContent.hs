module Semantic.Modules.Modal.ModalContent where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

data ModalContent ms = ModalContent_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , image :: Bool
    , scrolling :: Bool
    } deriving (Generic)

instance Default (ModalContent ms) where
    def = (G.to gdef) { as = Div }

pattern ModalContent :: Typeable ms => ModalContent ms -> View ms
pattern ModalContent mc = View mc

instance Typeable ms => Pure ModalContent ms where
    render ModalContent_ {..} =
        let
            cs = classes ++
                [ image # "image"
                , scrolling # "scrolling"
                , "content"
                ]

        in
            as
                ( ClassList cs
                : attributes
                ) 
                children
