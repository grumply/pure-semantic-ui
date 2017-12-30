module Semantic.Views.Card (module Semantic.Views.Card, module Export) where

import GHC.Generics as G
import Pure.View hiding (color,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Centered
import Semantic.Properties.Color
import Semantic.Properties.Fluid
import Semantic.Properties.Ref
import Semantic.Properties.Link
import Semantic.Properties.OnClick
import Semantic.Properties.Raised

import Semantic.Views.Card.CardContent as Export
import Semantic.Views.Card.CardDescription as Export

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

pattern Card :: Typeable ms => Card ms -> View ms
pattern Card a = View a

instance Typeable ms => Pure Card ms where
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
                ( ClassList cs
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

