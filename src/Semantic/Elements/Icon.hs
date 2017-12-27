module Semantic.Elements.Icon (module Semantic.Elements.Icon, module Export) where

import GHC.Generics as G
import Pure.View as View hiding (color,name)

import Semantic.Utils

import Semantic.Elements.Icon.IconGroup as Export

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Bordered
import Semantic.Properties.Circular
import Semantic.Properties.Classes
import Semantic.Properties.Color
import Semantic.Properties.Name

data Icon ms = Icon_
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
    } deriving (Generic)

instance Default (Icon ms) where
    def = (G.to gdef) { as = I }

pattern Icon :: Typeable ms => Icon ms -> View ms
pattern Icon i = View i

instance Typeable ms => Pure Icon ms where
    render Icon_ {..} =
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

instance HasAsProp (Icon ms) where
    type AsProp (Icon ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f i = i { as = f }

instance HasAttributesProp (Icon ms) where
    type Attribute (Icon ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs i = i { attributes = cs }

instance HasBorderedProp (Icon ms) where
    getBordered = bordered
    setBordered b i = i { bordered = b }

instance HasCircularProp (Icon ms) where
    getCircular = circular
    setCircular c i = i { circular = c }

instance HasNameProp (Icon ms) where
    getName = name
    setName n i = i { name = n }

instance HasClassesProp (Icon ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }

instance HasColorProp (Icon ms) where
    getColor = color
    setColor c i = i { color = c }