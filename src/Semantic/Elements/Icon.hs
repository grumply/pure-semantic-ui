module Semantic.Elements.Icon
  ( module Properties
  , module Tools
  , Icon(..), pattern Icon
  , Group(..), pattern Group
  ) where

import GHC.Generics as G
import Pure.View as View hiding (color,disabled,name)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasBorderedProp(..), pattern Bordered
  , HasCircularProp(..), pattern Circular
  , HasClassesProp(..), pattern Classes
  , HasColorProp(..), pattern Color
  , HasCornerProp(..), pattern Corner
  , HasDisabledProp(..), pattern Disabled
  , HasFittedProp(..), pattern Fitted
  , HasFlippedProp(..), pattern Flipped
  , HasInvertedProp(..), pattern Inverted
  , HasLinkProp(..), pattern Link
  , HasLoadingProp(..), pattern Loading
  , HasNameProp(..), pattern Name
  , HasRotatedProp(..), pattern Rotated
  , HasSizeProp(..), pattern Size
  )

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

pattern Icon :: Icon ms -> View ms
pattern Icon i = View i

instance Pure Icon ms where
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
                ( mergeClasses $ ClassList cs
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

instance HasCornerProp (Icon ms) where
    type CornerProp (Icon ms) = Bool
    getCorner = corner
    setCorner c i = i { corner = c }

instance HasDisabledProp (Icon ms) where
    getDisabled = disabled
    setDisabled d i = i { disabled = d }

instance HasFittedProp (Icon ms) where
    getFitted = fitted
    setFitted f i = i { fitted = f }

instance HasFlippedProp (Icon ms) where
    getFlipped = flipped
    setFlipped f i = i { flipped = f }

instance HasInvertedProp (Icon ms) where
    getInverted = inverted
    setInverted inv i = i { inverted = inv }

instance HasLinkProp (Icon ms) where
    getLink = link
    setLink l i = i { link = l }

instance HasLoadingProp (Icon ms) where
    getLoading = loading
    setLoading l i = i { loading = l }

instance HasRotatedProp (Icon ms) where
    getRotated = rotated
    setRotated r i = i { rotated = r }

instance HasSizeProp (Icon ms) where
    getSize = size
    setSize s i = i { size = s }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , children :: [View ms]
    , classes :: [Txt]
    , attributes :: [Feature ms]
    , size :: Txt
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = I }

pattern Group :: Group ms -> View ms
pattern Group ig = View ig

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( size
                : "icons"
                : classes
                )
        in as (mergeClasses $ ClassList cs : attributes) children

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
