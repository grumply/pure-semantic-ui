module Semantic.Collections.Form.FormField where

import GHC.Generics as G
import Pure.View hiding (disabled,inline,widths)
import qualified Pure.View as HTML

import Prelude hiding (error)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasDisabledProp(..), pattern Disabled
  , HasErrorProp(..), pattern Error
  , HasInlineProp(..), pattern Inline
  , HasRequiredProp(..), pattern Required
  , HasTypeProp(..), pattern Type
  , HasWidthsProp(..), pattern Widths
  )

data FormField ms = FormField_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , disabled :: Bool
    , error :: Bool
    , inline :: Bool
    , required :: Bool
    , _type :: Txt
    , widths :: Width
    } deriving (Generic)

instance Default (FormField ms) where
    def = (G.to gdef) { as = Div }

pattern FormField :: FormField ms -> View ms
pattern FormField ff = View ff

instance Pure FormField ms where
    render FormField_ {..} =
        let
            cs =
                ( disabled # "disabled"
                : error # "error"
                : inline # "inline"
                : required # "required"
                : widthProp widths "wide" def
                : "field"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (FormField ms) where
    type AsProp (FormField ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ff = ff { as = a }

instance HasAttributesProp (FormField ms) where
    type Attribute (FormField ms) = Feature ms
    getAttributes = attributes
    setAttributes as ff = ff { attributes = as }

instance HasChildrenProp (FormField ms) where
    type Child (FormField ms) = View ms
    getChildren = children
    setChildren cs ff = ff { children = cs }

instance HasClassesProp (FormField ms) where
    getClasses = classes
    setClasses cs ff = ff { classes = cs }

instance HasDisabledProp (FormField ms) where
    getDisabled = disabled
    setDisabled d ff = ff { disabled = d }

instance HasErrorProp (FormField ms) where
    getError = error
    setError e ff = ff { error = e }

instance HasInlineProp (FormField ms) where
    type InlineProp (FormField ms) = Bool
    getInline = inline
    setInline i ff = ff { inline = i }

instance HasRequiredProp (FormField ms) where
    getRequired = required
    setRequired r ff = ff { required = r }

instance HasTypeProp (FormField ms) where
    getType = _type
    setType t ff = ff { _type = t }

instance HasWidthsProp (FormField ms) where
    getWidths = widths
    setWidths w ff = ff { widths = w }
