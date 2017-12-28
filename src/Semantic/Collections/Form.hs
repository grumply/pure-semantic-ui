module Semantic.Collections.Form (module Semantic.Collections.Form, module Export) where

import GHC.Generics as G
import Pure.View hiding (name,onSubmit,widths,Form)
import qualified Pure.View as HTML

import Semantic.Utils
import Prelude hiding (error)

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Action
import Semantic.Properties.Error
import Semantic.Properties.Inverted
import Semantic.Properties.Loading
import Semantic.Properties.OnSubmit
import Semantic.Properties.Reply
import Semantic.Properties.Size
import Semantic.Properties.Success
import Semantic.Properties.Unstackable
import Semantic.Properties.Warning
import Semantic.Properties.Widths

import Semantic.Collections.Form.FormField as Export

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

pattern Form :: Typeable ms => Form ms -> View ms
pattern Form f = View f

instance Typeable ms => Pure Form ms where
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
                ( ClassList cs
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