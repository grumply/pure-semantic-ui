module Semantic.Modules.Modal.ModalContent where

import GHC.Generics as G
import Pure.View
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.IsImage
import Semantic.Properties.Scrolling

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
                ( mergeClasses $ ClassList cs
                : attributes
                ) 
                children

instance HasAsProp (ModalContent ms) where
    type AsProp (ModalContent ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f mc = mc { as = f }

instance HasAttributesProp (ModalContent ms) where
    type Attribute (ModalContent ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs mc = mc { attributes = cs }

instance HasChildrenProp (ModalContent ms) where
    type Child (ModalContent ms) = View ms
    getChildren = children
    setChildren cs mc = mc { children = cs }

instance HasClassesProp (ModalContent ms) where
    getClasses = classes
    setClasses cs mc = mc { classes = cs }

instance HasIsImageProp (ModalContent ms) where
    getIsImage = image
    setIsImage ii mc = mc { image = ii }

instance HasScrollingProp (ModalContent ms) where
    getScrolling = scrolling
    setScrolling s mc = mc { scrolling = s }