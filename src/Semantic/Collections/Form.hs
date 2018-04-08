module Semantic.Collections.Form where

import GHC.Generics as G
import Pure.View hiding (name,onSubmit,widths,Form,inline,disabled)
import qualified Pure.View as HTML

import Semantic.Utils
import Prelude hiding (error)

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActionProp(..), pattern Action
  , HasErrorProp(..), pattern Error
  , HasInvertedProp(..), pattern Inverted
  , HasLoadingProp(..), pattern Loading
  , HasOnSubmitProp(..), pattern OnSubmit
  , HasReplyProp(..), pattern Reply
  , HasSizeProp(..), pattern Size
  , HasSuccessProp(..), pattern Success
  , HasUnstackableProp(..), pattern Unstackable
  , HasWarningProp(..), pattern Warning
  , HasWidthsProp(..), pattern Widths
  , HasGroupedProp(..), pattern Grouped
  , HasInlineProp(..), pattern Inline
  , HasDisabledProp(..), pattern Disabled
  , HasErrorProp(..), pattern Error
  , HasRequiredProp(..), pattern Required
  , HasTypeProp(..), pattern Type
  )

data Form ms = Form_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , action :: Txt
    , error :: Bool
    , inverted :: Bool
    , loading :: Bool
    , onSubmit :: Ef ms IO ()
    , reply :: Bool
    , size :: Txt
    , success :: Bool
    , unstackable :: Bool
    , warning :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (Form ms) where
    def = (G.to gdef) { as = HTML.Form }

pattern Form :: Form ms -> View ms
pattern Form f = View f

instance Pure Form ms where
    render Form_ {..} =
        let
            cs =
                ( "ui"
                : size
                : error # "error"
                : inverted # "inverted"
                : loading # "loading"
                : reply # "reply"
                : success # "success"
                : unstackable # "unstackable"
                : warning # "warning"
                : widthProp widths def True
                : "form"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : Prop "action" action
                : onSubmit # (On "submit" def { preventDef = True } (\_ -> return $ Just onSubmit))
                : attributes
                )
                children

instance HasAsProp (Form ms) where
    type AsProp (Form ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a f = f { as = a }

instance HasAttributesProp (Form ms) where
    type Attribute (Form ms) = Feature ms
    getAttributes = attributes
    setAttributes as f = f { attributes = as }

instance HasChildrenProp (Form ms) where
    type Child (Form ms) = View ms
    getChildren = children
    setChildren cs f = f { children = cs }

instance HasClassesProp (Form ms) where
    getClasses = classes
    setClasses cs f = f { classes = cs }

instance HasActionProp (Form ms) where
    getAction = action
    setAction a f = f { action = a }

instance HasErrorProp (Form ms) where
    getError = error
    setError e f = f { error = e }

instance HasInvertedProp (Form ms) where
    getInverted = inverted
    setInverted i f = f { inverted = i }

instance HasLoadingProp (Form ms) where
    getLoading = loading
    setLoading l f = f { loading = l }

instance HasOnSubmitProp (Form ms) where
    type OnSubmitProp (Form ms) = Ef ms IO ()
    getOnSubmit = onSubmit
    setOnSubmit os f = f { onSubmit = os }

instance HasReplyProp (Form ms) where
    getReply = reply
    setReply r f = f { reply = r }

instance HasSizeProp (Form ms) where
    getSize = size
    setSize sz f = f { size = sz }

instance HasSuccessProp (Form ms) where
    getSuccess = success
    setSuccess s f = f { success = s }

instance HasUnstackableProp (Form ms) where
    getUnstackable = unstackable
    setUnstackable u f = f { unstackable = u }

instance HasWarningProp (Form ms) where
    getWarning = warning
    setWarning w f = f { warning = w }

instance HasWidthsProp (Form ms) where
    getWidths = widths
    setWidths w f = f { widths = w }

data Field ms = Field_
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

instance Default (Field ms) where
    def = (G.to gdef) { as = Div }

pattern Field :: Field ms -> View ms
pattern Field ff = View ff

instance Pure Field ms where
    render Field_ {..} =
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

instance HasAsProp (Field ms) where
    type AsProp (Field ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ff = ff { as = a }

instance HasAttributesProp (Field ms) where
    type Attribute (Field ms) = Feature ms
    getAttributes = attributes
    setAttributes as ff = ff { attributes = as }

instance HasChildrenProp (Field ms) where
    type Child (Field ms) = View ms
    getChildren = children
    setChildren cs ff = ff { children = cs }

instance HasClassesProp (Field ms) where
    getClasses = classes
    setClasses cs ff = ff { classes = cs }

instance HasDisabledProp (Field ms) where
    getDisabled = disabled
    setDisabled d ff = ff { disabled = d }

instance HasErrorProp (Field ms) where
    getError = error
    setError e ff = ff { error = e }

instance HasInlineProp (Field ms) where
    type InlineProp (Field ms) = Bool
    getInline = inline
    setInline i ff = ff { inline = i }

instance HasRequiredProp (Field ms) where
    getRequired = required
    setRequired r ff = ff { required = r }

instance HasTypeProp (Field ms) where
    getType = _type
    setType t ff = ff { _type = t }

instance HasWidthsProp (Field ms) where
    getWidths = widths
    setWidths w ff = ff { widths = w }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , grouped :: Bool
    , inline :: Bool
    , unstackable :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group fg = View fg

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( grouped # "grouped"
                : inline # "inline"
                : unstackable # "unstackable"
                : widthProp widths def True
                : "fields"
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
    setAs a fg = fg { as = a }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes as fg = fg { attributes = as }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs fg = fg { children = cs }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs fg = fg { classes = cs }

instance HasGroupedProp (Group ms) where
    getGrouped = grouped
    setGrouped g fg = fg { grouped = g }

instance HasInlineProp (Group ms) where
    type InlineProp (Group ms) = Bool
    getInline = inline
    setInline i fg = fg { inline = i }

instance HasUnstackableProp (Group ms) where
    getUnstackable = unstackable
    setUnstackable u fg = fg { unstackable = u }

instance HasWidthsProp (Group ms) where
    getWidths = widths
    setWidths w fg = fg { widths = w }
