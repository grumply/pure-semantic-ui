module Semantic.Views.Card
  ( module Properties
  , module Tools
  , Card(..), pattern Card
  , Content(..), pattern Content
  , Description(..), pattern Description
  , Group(..), pattern Group
  , Header(..), pattern Header
  , Meta(..), pattern Meta
  ) where

import GHC.Generics as G hiding (Meta)
import Pure.View hiding (color,onClick,textAlign,Content,Header,Meta,Description)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasCenteredProp(..), pattern Centered
  , HasColorProp(..), pattern Color
  , HasFluidProp(..), pattern Fluid
  , HasRefProp(..), pattern Ref
  , HasLinkProp(..), pattern Link
  , HasOnClickProp(..), pattern OnClick
  , HasRaisedProp(..), pattern Raised
  , HasExtraProp(..), pattern Extra
  , HasTextAlignProp(..), pattern TextAlign
  , HasDoublingProp(..), pattern Doubling
  , HasItemsPerRowProp(..), pattern ItemsPerRow
  , HasStackableProp(..), pattern Stackable
  )

data Card ms = Card_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , centered :: Bool
    , color :: Txt
    , fluid :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    , raised :: Bool
    } deriving (Generic)

instance Default (Card ms) where
    def = (G.to gdef) { as = Div }

pattern Card :: Card ms -> View ms
pattern Card a = View a

instance Pure Card ms where
    render Card_ {..} =
        let
            e = onClick ? A $ as
            cs =
                ( "ui"
                : color
                : centered # "centered"
                : fluid # "fluid"
                : link # "link"
                : raised # "raised"
                : "card"
                : classes
                )
        in
            e
                ( mergeClasses $ ClassList cs
                : ref
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasAsProp (Card ms) where
    type AsProp (Card ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a c = c { as = a }

instance HasAttributesProp (Card ms) where
    type Attribute (Card ms) = Feature ms
    getAttributes = attributes
    setAttributes as c = c { attributes = as }

instance HasChildrenProp (Card ms) where
    type Child (Card ms) = View ms
    getChildren = children
    setChildren cs c = c { children = cs }

instance HasClassesProp (Card ms) where
    getClasses = classes
    setClasses cs c = c { classes = cs }

instance HasCenteredProp (Card ms) where
    getCentered = centered
    setCentered c crd = crd { centered = c }

instance HasColorProp (Card ms) where
    getColor = color
    setColor c crd = crd { color = c }

instance HasFluidProp (Card ms) where
    getFluid = fluid
    setFluid f c = c { fluid = f }

instance HasRefProp (Card ms) where
    type RefProp (Card ms) = Feature ms
    getRef = ref
    setRef r c = c { ref = r }

instance HasLinkProp (Card ms) where
    getLink = link
    setLink l c = c { link = l }

instance HasOnClickProp (Card ms) where
    type OnClickProp (Card ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc c = c { onClick = oc }

instance HasRaisedProp (Card ms) where
    getRaised = raised
    setRaised r c = c { raised = r }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , extra :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content cc = View cc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( extra # "extra"
                : textAlign
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
    setAs a cc = cc { as = a }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes as cc = cc { attributes = as }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs cc = cc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs cc = cc { classes = cs }

instance HasExtraProp (Content ms) where
    getExtra = extra
    setExtra e cc = cc { extra = e }

instance HasTextAlignProp (Content ms) where
    getTextAlign = textAlign
    setTextAlign ta cc = cc { textAlign = ta }

data Description ms = Description_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Description ms) where
    def = (G.to gdef) { as = Div }

pattern Description :: Description ms -> View ms
pattern Description cd = View cd

instance Pure Description ms where
    render Description_ {..} =
        let
            cs =
                ( textAlign
                : "description"
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
    setAs a cd = cd { as = a }

instance HasAttributesProp (Description ms) where
    type Attribute (Description ms) = Feature ms
    getAttributes = attributes
    setAttributes as cd = cd { attributes = as }

instance HasChildrenProp (Description ms) where
    type Child (Description ms) = View ms
    getChildren = children
    setChildren cs cd = cd { children = cs }

instance HasClassesProp (Description ms) where
    getClasses = classes
    setClasses cs cd = cd { classes = cs }

instance HasTextAlignProp (Description ms) where
    getTextAlign = textAlign
    setTextAlign ta cd = cd { textAlign = ta }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , doubling :: Bool
    , itemsPerRow :: Width
    , stackable :: Bool
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group cg = View cg

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : doubling # "doubling"
                : stackable # "stackable"
                : textAlign
                : widthProp def width def
                : "cards"
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
    setAs a cg = cg { as = a }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes as cg = cg { attributes = as }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs cg = cg { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs cg = cg { classes = cs }

instance HasDoublingProp (Group ms) where
    getDoubling = doubling
    setDoubling d cg = cg { doubling = d }

instance HasItemsPerRowProp (Group ms) where
    getItemsPerRow = itemsPerRow
    setItemsPerRow ipr cg = cg { itemsPerRow = ipr }

instance HasStackableProp (Group ms) where
    type StackableProp (Group ms) = Bool
    getStackable = stackable
    setStackable s cg = cg { stackable = s }

instance HasTextAlignProp (Group ms) where
    getTextAlign = textAlign
    setTextAlign ta cc = cc { textAlign = ta }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header ch = View ch

instance Pure Header ms where
    render Header_ {..} =
        let
            cs =
                ( textAlign
                : "header"
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
    setAs a ch = ch { as = a }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes as ch = ch { attributes = as }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs ch = ch { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs ch = ch { classes = cs }

instance HasTextAlignProp (Header ms) where
    getTextAlign = textAlign
    setTextAlign ta ch = ch { textAlign = ta }

data Meta ms = Meta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , textAlign :: Txt
    } deriving (Generic)

instance Default (Meta ms) where
    def = (G.to gdef) { as = Div }

pattern Meta :: Meta ms -> View ms
pattern Meta cm = View cm

instance Pure Meta ms where
    render Meta_ {..} =
        let
            cs =
                ( textAlign
                : "meta"
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
    setAs a cm = cm { as = a }

instance HasAttributesProp (Meta ms) where
    type Attribute (Meta ms) = Feature ms
    getAttributes = attributes
    setAttributes as cm = cm { attributes = as }

instance HasChildrenProp (Meta ms) where
    type Child (Meta ms) = View ms
    getChildren = children
    setChildren cs cm = cm { children = cs }

instance HasClassesProp (Meta ms) where
    getClasses = classes
    setClasses cs cm = cm { classes = cs }

instance HasTextAlignProp (Meta ms) where
    getTextAlign = textAlign
    setTextAlign ta cm = cm { textAlign = ta }
