module Semantic.Elements.Image (module Semantic.Elements.Image, module Export) where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Elements.Image.ImageGroup as Export

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Avatar
import Semantic.Extensions.Bordered
import Semantic.Extensions.Children
import Semantic.Extensions.Classes

data Image ms = Image_
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
    , ui :: Bool
    , verticalAlign :: Txt
    , wrapped :: Bool
    } deriving (Generic)

instance Default (Image ms) where
    def = (G.to gdef) { ui = True, as = Img }

pattern Image :: Typeable ms => Image ms -> View ms
pattern Image i = View i

instance Typeable ms => Pure Image ms where
    render Image_ {..} =
        let
            cs =
                ( ui # "ui"
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
                : "image"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                children

instance HasAvatar (Image ms) where
    getAvatar = avatar
    setAvatar a i = i { avatar = a }

instance HasAs (Image ms) where
    type Constructor (Image ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f i = i { as = f }

instance HasAttributes (Image ms) where
    type Attribute (Image ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs i = i { attributes = cs }

instance HasBordered (Image ms) where
    getBordered = bordered
    setBordered b i = i { bordered = b }

instance HasChildren (Image ms) where
    type Child (Image ms) = View ms
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasClasses (Image ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }