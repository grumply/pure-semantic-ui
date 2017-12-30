module Semantic.Views.Item.ItemImage where

import GHC.Generics as G
import Pure.View as View hiding (disabled,hidden,inline,verticalAlign)

import Semantic.Utils

data ItemImage ms = ItemImage_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , avatar :: Bool
    , bordered :: Bool
    , centered :: Bool
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , disabled :: Bool
    , floated :: Txt
    , fluid :: Bool
    , hidden :: Bool
    , inline :: Bool
    , rounded :: Bool
    , size :: Txt
    , spaced :: Maybe Txt
    , verticalAlign :: Txt
    , wrapped :: Bool
    } deriving (Generic)

instance Default (ItemImage ms) where
    def = (G.to gdef) { as = Img }

pattern ItemImage :: Typeable ms => ItemImage ms -> View ms
pattern ItemImage i = View i

instance Typeable ms => Pure ItemImage ms where
    render ItemImage_ {..} =
        let
            cs =
                ( size # "ui"
                : size
                : avatar # "avatar"
                : bordered # "bordered"
                : circular # "circular"
                : centered # "centered"
                : disabled # "disabled"
                : fluid # "fluid"
                : hidden # "hidden"
                : inline # "inline"
                : rounded # "rounded"
                : useKeyOrValueAndKey spaced "spaced"
                : floated # ("floated" <<>> floated)
                : verticalAlign # ("aligned" <<>> verticalAlign)
                : "ItemImage"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children
