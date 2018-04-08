module Semantic.Elements.Image where

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

pattern Image :: Image ms -> View ms
pattern Image i = View i

instance Pure Image ms where
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
                ( mergeClasses $ ClassList cs
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

instance HasUIProp (Image ms) where
    getUI = ui
    setUI x i = i { ui = x }

instance HasVerticalAlignProp (Image ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va i = i { verticalAlign = va }

instance HasWrappedProp (Image ms) where
    getWrapped = wrapped
    setWrapped w i = i { wrapped = w }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group ig = View ig

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : size
                : classes
                ) ++ [ "images" ]
        in 
            as (mergeClasses $ ClassList cs : attributes) children

instance HasAsProp (Group ms) where
    type AsProp (Group ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ig = ig { as = f }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs ig = ig { attributes = cs }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs ig = ig { classes = cs }

instance HasSizeProp (Group ms) where
    getSize = size
    setSize s ig = ig { size = s }
