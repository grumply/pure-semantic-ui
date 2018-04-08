module Semantic.Modules.Accordion
  ( module Properties
  , module Tools
  , Accordion(..), pattern Accordion
  , Subaccordion(..), pattern Subaccordion
  , Content(..), pattern Content
  , Title(..), pattern Title
  ) where

import GHC.Generics as G
import Pure.View hiding (styled,onClick,active,Content,Title)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasFluidProp(..), pattern Fluid
  , HasInvertedProp(..), pattern Inverted
  , HasStyledProp(..), pattern Styled
  , HasActiveProp(..), pattern Active
  , HasIndexProp(..), pattern Index
  , HasOnClickProp(..), pattern OnClick
  )

data Accordion ms = Accordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , fluid :: Bool
    , inverted :: Bool
    , styled :: Bool
    } deriving (Generic)

instance Default (Accordion ms) where
    def = (G.to gdef) { as = Div }

pattern Accordion :: Accordion ms -> View ms
pattern Accordion a = View a

instance Pure Accordion ms where
    render Accordion_ {..} =
        let
            cs =
                ( "ui"
                : "accordion"
                : fluid # "fluid"
                : inverted # "inverted"
                : styled # "styled"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Accordion ms) where
    type AsProp (Accordion ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a acc = acc { as = a }

instance HasAttributesProp (Accordion ms) where
    type Attribute (Accordion ms) = Feature ms
    getAttributes = attributes
    setAttributes as a = a { attributes = as }

instance HasChildrenProp (Accordion ms) where
    type Child (Accordion ms) = View ms
    getChildren = children
    setChildren cs a = a { children = cs }

instance HasClassesProp (Accordion ms) where
    getClasses = classes
    setClasses cs a = a { classes = cs }

instance HasFluidProp (Accordion ms) where
    getFluid = fluid
    setFluid f a = a { fluid = f }

instance HasInvertedProp (Accordion ms) where
    getInverted = inverted
    setInverted i a = a { inverted = i }

instance HasStyledProp (Accordion ms) where
    getStyled = styled
    setStyled s a = a { styled = s }

data Subaccordion ms = Subaccordion_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Subaccordion ms) where
    def = (G.to gdef) { as = Div }

pattern Subaccordion :: Subaccordion ms -> View ms
pattern Subaccordion a = View a

instance Pure Subaccordion ms where
    render Subaccordion_ {..} =
        let
            cs =
                ( "accordion"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Subaccordion ms) where
    type AsProp (Subaccordion ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a acc = acc { as = a }

instance HasAttributesProp (Subaccordion ms) where
    type Attribute (Subaccordion ms) = Feature ms
    getAttributes = attributes
    setAttributes as a = a { attributes = as }

instance HasChildrenProp (Subaccordion ms) where
    type Child (Subaccordion ms) = View ms
    getChildren = children
    setChildren cs a = a { children = cs }

instance HasClassesProp (Subaccordion ms) where
    getClasses = classes
    setClasses cs a = a { classes = cs }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content ac = View ac

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( "content"
                : active # "active"
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
    setAs a ac = ac { as = a }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes as ac = ac { attributes = as }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs ac = ac { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs ac = ac { classes = cs }

instance HasActiveProp (Content ms) where
    getActive = active
    setActive a ac = ac { active = a }

data Title ms = Title_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , index :: Int
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (Title ms) where
    def = (G.to gdef) { as = Div }

pattern Title :: Title ms -> View ms
pattern Title at = View at

instance Pure Title ms where
    render Title_ {..} =
        let
            cs =
                ( active # "active"
                : "title"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : onClick # (On "click" def $ \_ -> return (Just onClick))
                : attributes
                )
                children

instance HasAsProp (Title ms) where
    type AsProp (Title ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a at = at { as = a }

instance HasAttributesProp (Title ms) where
    type Attribute (Title ms) = Feature ms
    getAttributes = attributes
    setAttributes as at = at { attributes = as }

instance HasChildrenProp (Title ms) where
    type Child (Title ms) = View ms
    getChildren = children
    setChildren cs at = at { children = cs }

instance HasClassesProp (Title ms) where
    getClasses = classes
    setClasses cs at = at { classes = cs }

instance HasActiveProp (Title ms) where
    getActive = active
    setActive a at = at { active = a }

instance HasIndexProp (Title ms) where
    getIndex = index
    setIndex i at = at { index = i }

instance HasOnClickProp (Title ms) where
    type OnClickProp (Title ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc at = at { onClick = oc }
