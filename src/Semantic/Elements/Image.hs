module Semantic.Elements.Image (module Semantic.Elements.Image, module Export) where

import GHC.Generics as G
import Pure.View as View hiding (disabled,hidden,inline)

import Semantic.Utils

import Semantic.Elements.Image.ImageGroup as Export

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Avatar
import Semantic.Properties.Bordered
import Semantic.Properties.Centered
import Semantic.Properties.Children
import Semantic.Properties.Circular
import Semantic.Properties.Classes
import Semantic.Properties.Disabled
import Semantic.Properties.Floated
import Semantic.Properties.Fluid
import Semantic.Properties.Hidden
import Semantic.Properties.Inline
import Semantic.Properties.Rounded
import Semantic.Properties.Size
import Semantic.Properties.Spaced

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

instance HasAvatarProp (Image ms) where
    getAvatar = avatar
    setAvatar a i = i { avatar = a }

instance HasAsProp (Image ms) where
    type AsProp (Image ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f i = i { as = f }

instance HasAttributesProp (Image ms) where
    type Attribute (Image ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs i = i { attributes = cs }

instance HasBorderedProp (Image ms) where
    getBordered = bordered
    setBordered b i = i { bordered = b }

instance HasCenteredProp (Image ms) where
    getCentered = centered
    setCentered c i = i { centered = c }

instance HasChildrenProp (Image ms) where
    type Child (Image ms) = View ms
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasCircularProp (Image ms) where
    getCircular = circular
    setCircular c i = i { circular = c }

instance HasClassesProp (Image ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }

instance HasDisabledProp (Image ms) where
    getDisabled = disabled
    setDisabled d i = i { disabled = d }

instance HasFloatedProp (Image ms) where
    getFloated = floated
    setFloated f i = i { floated = f }

instance HasFluidProp (Image ms) where
    getFluid = fluid
    setFluid f i = i { fluid = f }

instance HasInlineProp (Image ms) where
    type InlineProp (Image ms) = Bool
    getInline = inline
    setInline inl i = i { inline = inl }

instance HasHiddenProp (Image ms) where
    getHidden = hidden
    setHidden h i = i { hidden = h }

instance HasRoundedProp (Image ms) where
    getRounded = rounded
    setRounded r i = i { rounded = r }

instance HasSizeProp (Image ms) where
    getSize = size
    setSize s i = i { size = s }

instance HasSpacedProp (Image ms) where
    getSpaced = spaced
    setSpaced s i = i { spaced = s }