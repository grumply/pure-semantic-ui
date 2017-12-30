module Semantic.Views.Feed (module Semantic.Views.Feed, module Export) where

import GHC.Generics as G
import Pure.View

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Size

import Semantic.Views.Feed.FeedContent as Export
import Semantic.Views.Feed.FeedDate as Export
import Semantic.Views.Feed.FeedEvent as Export
import Semantic.Views.Feed.FeedExtra as Export
import Semantic.Views.Feed.FeedLabel as Export
import Semantic.Views.Feed.FeedLike as Export
import Semantic.Views.Feed.FeedMeta as Export
data Feed ms = Feed_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Feed ms) where
    def = (G.to gdef) { as = Div }

pattern Feed :: Typeable ms => Feed ms -> View ms
pattern Feed f = View f

instance Typeable ms => Pure Feed ms where
    render Feed_ {..} =
        let
            cs =
                ( "ui"
                : size
                : "feed"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAsProp (Feed ms) where
    type AsProp (Feed ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a f = f { as = a }

instance HasAttributesProp (Feed ms) where
    type Attribute (Feed ms) = Feature ms
    getAttributes = attributes
    setAttributes as f = f { attributes = as }

instance HasChildrenProp (Feed ms) where
    type Child (Feed ms) = View ms
    getChildren = children
    setChildren cs f = f { children = cs }

instance HasClassesProp (Feed ms) where
    getClasses = classes
    setClasses cs f = f { classes = cs }

instance HasSizeProp (Feed ms) where
    getSize = size
    setSize s f = f { size = s }