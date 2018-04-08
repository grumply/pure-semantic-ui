module Semantic.Views.Comment.CommentAction where

import GHC.Generics as G
import Pure.View hiding (active)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActiveProp(..), pattern Active
  )

data CommentAction ms = CommentAction_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    } deriving (Generic)

instance Default (CommentAction ms) where
    def = (G.to gdef) { as = A }

pattern CommentAction :: CommentAction ms -> View ms
pattern CommentAction ca = View ca

instance Pure CommentAction ms where
    render CommentAction_ {..} =
        let
            cs =
                ( active # "active"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (CommentAction ms) where
    type AsProp (CommentAction ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ca = ca { as = a }

instance HasAttributesProp (CommentAction ms) where
    type Attribute (CommentAction ms) = Feature ms
    getAttributes = attributes
    setAttributes as ca = ca { attributes = as }

instance HasChildrenProp (CommentAction ms) where
    type Child (CommentAction ms) = View ms
    getChildren = children
    setChildren cs ca = ca { children = cs }

instance HasClassesProp (CommentAction ms) where
    getClasses = classes
    setClasses cs ca = ca { classes = cs }

instance HasActiveProp (CommentAction ms) where
    getActive = active
    setActive a ca = ca { active = a }
