module Semantic.Collections.Message.MessageHeader where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  )

data MessageHeader ms = MessageHeader_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (MessageHeader ms) where
    def = (G.to gdef) { as = Div }

pattern MessageHeader :: MessageHeader ms -> View ms
pattern MessageHeader mh = View mh

instance Pure MessageHeader ms where
    render MessageHeader_ {..} =
        let
            cs =
                ( "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (MessageHeader ms) where
    type AsProp (MessageHeader ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mh = mh { as = a }

instance HasAttributesProp (MessageHeader ms) where
    type Attribute (MessageHeader ms) = Feature ms
    getAttributes = attributes
    setAttributes as mh = mh { attributes = as }

instance HasChildrenProp (MessageHeader ms) where
    type Child (MessageHeader ms) = View ms
    getChildren = children
    setChildren cs mh = mh { children = cs }

instance HasClassesProp (MessageHeader ms) where
    getClasses = classes
    setClasses cs mh = mh { classes = cs }

    
