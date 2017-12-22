module Semantic.Elements.Icon (module Semantic.Elements.Icon, module Export) where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Elements.Icon.IconGroup as Export

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