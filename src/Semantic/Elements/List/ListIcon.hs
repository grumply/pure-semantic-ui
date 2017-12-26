module Semantic.Elements.List.ListIcon where

import GHC.Generics as G
import Pure.View hiding (name,verticalAlign)

import Semantic.Utils

import Semantic.Extensions.Attributes
import Semantic.Extensions.Name

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

instance HasAttributes (ListIcon ms) where
    type Attribute (ListIcon ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs li = li { attributes = cs }

instance HasName (ListIcon ms) where
    getName = name
    setName n li = li { name = n }