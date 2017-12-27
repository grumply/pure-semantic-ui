module Semantic.Elements.List.ListIcon where

import GHC.Generics as G
import Pure.View hiding (color,disabled,name,verticalAlign)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Bordered
import Semantic.Properties.Circular
import Semantic.Properties.Classes
import Semantic.Properties.Color
import Semantic.Properties.Corner
import Semantic.Properties.Disabled
import Semantic.Properties.Fitted
import Semantic.Properties.Flipped
import Semantic.Properties.Inverted
import Semantic.Properties.Link
import Semantic.Properties.Loading
import Semantic.Properties.Name
import Semantic.Properties.Rotated
import Semantic.Properties.Size
import Semantic.Properties.VerticalAlign

data ListIcon ms = ListIcon_ 
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , bordered :: Bool
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , corner :: Bool
    , disabled :: Bool
    , fitted :: Bool
    , flipped :: Txt
    , inverted :: Bool
    , link :: Bool
    , loading :: Bool
    , name :: Txt
    , rotated :: Txt
    , size :: Txt 
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (ListIcon ms) where
    def = (G.to gdef) { as = I }

pattern ListIcon :: Typeable ms => ListIcon ms -> View ms
pattern ListIcon li = View li

instance Typeable ms => Pure ListIcon ms where
    render ListIcon_ {..} = 
        let
            cs =
                ( color
                : name
                : cond size
                : bordered # "bordered"
                : circular # "circular"
                : corner # "corner"
                : disabled # "disabled"
                : fitted # "fitted"
                : inverted # "inverted"
                : link # "link"
                : loading # "loading"
                : flipped # ("flipped" <<>> flipped)
                : rotated # ("rotated" <<>> rotated)
                : verticalAlign # (verticalAlign <>> "aligned")
                : "icon"
                : classes
                )
        in
            as
                ( ClassList cs
                : Attr "aria-hidden" "true"
                : attributes
                )
                []

instance HasAsProp (ListIcon ms) where
    type AsProp (ListIcon ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f li = li { as = f }

instance HasAttributesProp (ListIcon ms) where
    type Attribute (ListIcon ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs li = li { attributes = cs }

instance HasBorderedProp (ListIcon ms) where
    getBordered = bordered
    setBordered b li = li { bordered = b }

instance HasCircularProp (ListIcon ms) where
    getCircular = circular
    setCircular c li = li { circular = c }

instance HasNameProp (ListIcon ms) where
    getName = name
    setName n li = li { name = n }

instance HasClassesProp (ListIcon ms) where
    getClasses = classes
    setClasses cs li = li { classes = cs }

instance HasColorProp (ListIcon ms) where
    getColor = color
    setColor c li = li { color = c }

instance HasCornerProp (ListIcon ms) where
    type CornerProp (ListIcon ms) = Bool
    getCorner = corner
    setCorner c li = li { corner = c }

instance HasDisabledProp (ListIcon ms) where
    getDisabled = disabled
    setDisabled d li = li { disabled = d }

instance HasFittedProp (ListIcon ms) where
    getFitted = fitted
    setFitted f li = li { fitted = f }

instance HasFlippedProp (ListIcon ms) where
    getFlipped = flipped
    setFlipped f li = li { flipped = f }

instance HasInvertedProp (ListIcon ms) where
    getInverted = inverted
    setInverted i li = li { inverted = i }

instance HasLinkProp (ListIcon ms) where
    getLink = link
    setLink l li = li { link = l }

instance HasLoadingProp (ListIcon ms) where
    getLoading = loading
    setLoading l li = li { loading = l }

instance HasRotatedProp (ListIcon ms) where
    getRotated = rotated
    setRotated r li = li { rotated = r }

instance HasSizeProp (ListIcon ms) where
    getSize = size
    setSize s li = li { size = s }

instance HasVerticalAlignProp (ListIcon ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va li = li { verticalAlign = va }