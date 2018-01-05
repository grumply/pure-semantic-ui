module Semantic.Views.Feed.FeedExtra where

import GHC.Generics as G
import Pure.View hiding (text)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.IsText

import Semantic.Elements.Image

data FeedExtra ms = FeedExtra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (FeedExtra ms) where
    def = (G.to gdef) { as = Div }

pattern FeedExtra :: Typeable ms => FeedExtra ms -> View ms
pattern FeedExtra fe = View fe

instance Typeable ms => Pure FeedExtra ms where
    render FeedExtra_ {..} =
        let
            image = foldPures (\(Image_ {}) -> const True) False children

            cs =
                ( image # "images"
                : text # "text"
                : "extra"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FeedExtra ms) where
    type AsProp (FeedExtra ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fe = fe { as = a }

instance HasAttributesProp (FeedExtra ms) where
    type Attribute (FeedExtra ms) = Feature ms
    getAttributes = attributes
    setAttributes as fe = fe { attributes = as }

instance HasChildrenProp (FeedExtra ms) where
    type Child (FeedExtra ms) = View ms
    getChildren = children
    setChildren cs fe = fe { children = cs }

instance HasClassesProp (FeedExtra ms) where
    getClasses = classes
    setClasses cs fe = fe { classes = cs }

instance HasIsTextProp (FeedExtra ms) where
    getIsText = text
    setIsText it fe = fe { text = it }