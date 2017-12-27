module Semantic.Elements.Icon (module Semantic.Elements.Icon, module Export) where

import GHC.Generics as G
import Pure.View as View hiding (name)

import Semantic.Utils

import Semantic.Elements.Icon.IconGroup as Export

import Semantic.Extensions.As
import Semantic.Extensions.Attributes
import Semantic.Extensions.Classes
import Semantic.Extensions.Name

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

instance HasAs (Icon ms) where
    type Constructor (Icon ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f i = i { as = f }

instance HasAttributes (Icon ms) where
    type Attribute (Icon ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs i = i { attributes = cs }

instance HasName (Icon ms) where
    getName = name
    setName n i = i { name = n }

instance HasClasses (Icon ms) where
    getClasses = classes
    setClasses cs i = i { classes = cs }