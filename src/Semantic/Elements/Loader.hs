module Semantic.Elements.Loader where

import GHC.Generics as G
import Pure.View hiding (verticalAlign)

import Semantic.Utils

import Semantic.Extensions.Attributes
import Semantic.Extensions.Children

data Loader ms = Loader_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , indeterminate :: Bool
    , inline :: Maybe Txt
    , inverted :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default (Loader ms) where
    def = (G.to gdef) { as = Div }

pattern Loader :: Typeable ms => Loader ms -> View ms
pattern Loader l = View l

instance Typeable ms => Pure Loader ms where
    render Loader_ {..} =
        let
            cs =
                ( "ui"
                : size
                : active # "active"
                : disabled # "disabled"
                : indeterminate # "indeterminate"
                : inverted # "inverted"
                : children # "text"
                : may (<>> "inline") inline
                : "loader"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAttributes (Loader ms) where
    type Attribute (Loader ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs l = l { attributes = cs }

instance HasChildren (Loader ms) where
    type Child (Loader ms) = View ms
    getChildren = children
    setChildren cs l = l { children = cs }


