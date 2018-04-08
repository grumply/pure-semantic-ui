module Semantic.Views.Item.ItemImage where

import GHC.Generics as G
import Pure.View as View hiding (disabled,hidden,inline,verticalAlign)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasAvatarProp(..), pattern Avatar
  , HasBorderedProp(..), pattern Bordered
  , HasCenteredProp(..), pattern Centered
  , HasChildrenProp(..), pattern Children
  , HasCircularProp(..), pattern Circular
  , HasClassesProp(..), pattern Classes
  , HasDisabledProp(..), pattern Disabled
  , HasFloatedProp(..), pattern Floated
  , HasFluidProp(..), pattern Fluid
  , HasHiddenProp(..), pattern Hidden
  , HasInlineProp(..), pattern Inline
  , HasRoundedProp(..), pattern Rounded
  , HasSizeProp(..), pattern Size
  , HasSpacedProp(..), pattern Spaced
  , HasUIProp(..), pattern UI
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasWrappedProp(..), pattern Wrapped
  )

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

pattern ItemImage :: ItemImage ms -> View ms
pattern ItemImage i = View i

instance Pure ItemImage ms where
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
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAvatarProp (ItemImage ms) where
    getAvatar = avatar
    setAvatar a i = i { avatar = a }

instance HasAsProp (ItemImage ms) where
    type AsProp (ItemImage ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f i = i { as = f }

instance HasAttributesProp (ItemImage ms) where
    type Attribute (ItemImage ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs i = i { attributes = cs }

instance HasBorderedProp (ItemImage ms) where
    getBordered = bordered
    setBordered b i = i { bordered = b }

instance HasCenteredProp (ItemImage ms) where
    getCentered = centered
    setCentered c i = i { centered = c }

instance HasChildrenProp (ItemImage ms) where
    type Child (ItemImage ms) = View ms
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasCircularProp (ItemImage ms) where
    getCircular = circular
    setCircular c i = i { circular = c }

instance HasClassesProp (ItemImage ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }

instance HasDisabledProp (ItemImage ms) where
    getDisabled = disabled
    setDisabled d i = i { disabled = d }

instance HasFloatedProp (ItemImage ms) where
    getFloated = floated
    setFloated f i = i { floated = f }

instance HasFluidProp (ItemImage ms) where
    getFluid = fluid
    setFluid f i = i { fluid = f }

instance HasInlineProp (ItemImage ms) where
    type InlineProp (ItemImage ms) = Bool
    getInline = inline
    setInline inl i = i { inline = inl }

instance HasHiddenProp (ItemImage ms) where
    getHidden = hidden
    setHidden h i = i { hidden = h }

instance HasRoundedProp (ItemImage ms) where
    getRounded = rounded
    setRounded r i = i { rounded = r }

instance HasSizeProp (ItemImage ms) where
    getSize = size
    setSize s i = i { size = s }

instance HasSpacedProp (ItemImage ms) where
    getSpaced = spaced
    setSpaced s i = i { spaced = s }

instance HasVerticalAlignProp (ItemImage ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va i = i { verticalAlign = va }

instance HasWrappedProp (ItemImage ms) where
    getWrapped = wrapped
    setWrapped w i = i { wrapped = w }
