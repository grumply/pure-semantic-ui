module Semantic.Views.Item
  ( module Properties
  , module Tools
  , Item(..), pattern Item
  , Content(..), pattern Content
  , Description(..), pattern Description
  , Extra(..), pattern Extra
  , Group(..), pattern Group
  , Header(..), pattern Header
  , Image(..), pattern Image
  , Meta(..), pattern Meta
  ) where

import GHC.Generics as G hiding (Meta)
import Pure.View hiding (verticalAlign,disabled,inline,hidden,Content,Description,Header,Meta)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasDividedProp(..), pattern Divided
  , HasLinkProp(..), pattern Link
  , HasRelaxedProp(..), pattern Relaxed
  , HasUnstackableProp(..), pattern Unstackable
  , HasAvatarProp(..), pattern Avatar
  , HasBorderedProp(..), pattern Bordered
  , HasCenteredProp(..), pattern Centered
  , HasCircularProp(..), pattern Circular
  , HasDisabledProp(..), pattern Disabled
  , HasFloatedProp(..), pattern Floated
  , HasFluidProp(..), pattern Fluid
  , HasHiddenProp(..), pattern Hidden
  , HasInlineProp(..), pattern Inline
  , HasRoundedProp(..), pattern Rounded
  , HasSizeProp(..), pattern Size
  , HasSpacedProp(..), pattern Spaced
  , HasUIProp(..), pattern UI
  , HasWrappedProp(..), pattern Wrapped
  )

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Item ms -> View ms
pattern Item i = View i

instance Pure Item ms where
    render Item_ {..} =
        let
            cs =
                ( "item"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Item ms) where
    type AsProp (Item ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a i = i { as = a }

instance HasAttributesProp (Item ms) where
    type Attribute (Item ms) = Feature ms
    getAttributes = attributes
    setAttributes as i = i { attributes = as }

instance HasChildrenProp (Item ms) where
    type Child (Item ms) = View ms
    getChildren = children
    setChildren cs i = i { children = cs }

instance HasClassesProp (Item ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content ic = View ic

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( verticalAlign
                : "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ic = ic { as = a }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes as ic = ic { attributes = as }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs ic = ic { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs ic = ic { classes = cs }

instance HasVerticalAlignProp (Content ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va ic = ic { verticalAlign = va }

data Description ms = Description_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Description ms) where
    def = (G.to gdef) { as = Div }

pattern Description :: Description ms -> View ms
pattern Description id = View id

instance Pure Description ms where
    render Description_ {..} =
        let
            cs =
                ( "description"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Description ms) where
    type AsProp (Description ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a id = id { as = a }

instance HasAttributesProp (Description ms) where
    type Attribute (Description ms) = Feature ms
    getAttributes = attributes
    setAttributes as id = id { attributes = as }

instance HasChildrenProp (Description ms) where
    type Child (Description ms) = View ms
    getChildren = children
    setChildren cs id = id { children = cs }

instance HasClassesProp (Description ms) where
    getClasses = classes
    setClasses cs id = id { classes = cs }

data Extra ms = Extra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Extra ms) where
    def = (G.to gdef) { as = Div }

pattern Extra :: Extra ms -> View ms
pattern Extra ie = View ie

instance Pure Extra ms where
    render Extra_ {..} =
        let
            cs =
                ( "extra"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Extra ms) where
    type AsProp (Extra ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ie = ie { as = a }

instance HasAttributesProp (Extra ms) where
    type Attribute (Extra ms) = Feature ms
    getAttributes = attributes
    setAttributes as ie = ie { attributes = as }

instance HasChildrenProp (Extra ms) where
    type Child (Extra ms) = View ms
    getChildren = children
    setChildren cs ie = ie { children = cs }

instance HasClassesProp (Extra ms) where
    getClasses = classes
    setClasses cs ie = ie { classes = cs }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , divided :: Bool
    , link :: Bool
    , relaxed :: Maybe Txt
    , unstackable :: Bool
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
                : divided # "divided"
                : link # "link"
                : unstackable # "unstackable"
                : may (<>> "relaxed") relaxed
                : "items"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Group ms) where
    type AsProp (Group ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ig = ig { as = a }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes as ig = ig { attributes = as }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs ig = ig { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs ig = ig { classes = cs }

instance HasDividedProp (Group ms) where
    getDivided = divided
    setDivided d ig = ig { divided = d }

instance HasLinkProp (Group ms) where
    getLink = link
    setLink l ig = ig { link = l }

instance HasRelaxedProp (Group ms) where
    getRelaxed = relaxed
    setRelaxed r ig = ig { relaxed = r }

instance HasUnstackableProp (Group ms) where
    getUnstackable = unstackable
    setUnstackable u ig = ig { unstackable = u }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header ih = View ih

instance Pure Header ms where
    render Header_ {..} =
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

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ih = ih { as = a }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes as ih = ih { attributes = as }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs ih = ih { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs ih = ih { classes = cs }

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
    , verticalAlign :: Txt
    , wrapped :: Bool
    } deriving (Generic)

instance Default (Image ms) where
    def = (G.to gdef) { as = Img }

pattern Image :: Image ms -> View ms
pattern Image i = View i

instance Pure Image ms where
    render Image_ {..} =
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
                : "Image"
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

instance HasVerticalAlignProp (Image ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va i = i { verticalAlign = va }

instance HasWrappedProp (Image ms) where
    getWrapped = wrapped
    setWrapped w i = i { wrapped = w }

data Meta ms = Meta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Meta ms) where
    def = (G.to gdef) { as = Div }

pattern Meta :: Meta ms -> View ms
pattern Meta im = View im

instance Pure Meta ms where
    render Meta_ {..} =
        let
            cs =
                ( "meta"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Meta ms) where
    type AsProp (Meta ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a im = im { as = a }

instance HasAttributesProp (Meta ms) where
    type Attribute (Meta ms) = Feature ms
    getAttributes = attributes
    setAttributes as im = im { attributes = as }

instance HasChildrenProp (Meta ms) where
    type Child (Meta ms) = View ms
    getChildren = children
    setChildren cs im = im { children = cs }

instance HasClassesProp (Meta ms) where
    getClasses = classes
    setClasses cs im = im { classes = cs }
