module Semantic.Form
  ( module Properties
  , module Tools
  , Form(..), pattern Form
  , Field(..), pattern Field
  , Group(..), pattern Group
  ) where

import GHC.Generics as G
import Pure.View hiding (name,onSubmit,widths,Form,inline,disabled,Action)
import qualified Pure.View as HTML

import Semantic.Utils
import Prelude hiding (error)

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>), (!), (%) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Action, Action(..)
  , pattern Error, Error(..)
  , pattern Inverted, Inverted(..)
  , pattern Loading, Loading(..)
  , pattern OnSubmit, OnSubmit(..)
  , pattern Reply, Reply(..)
  , pattern Size, Size(..)
  , pattern Success, Success(..)
  , pattern Unstackable, Unstackable(..)
  , pattern Warning, Warning(..)
  , pattern Widths, Widths(..)
  , pattern Grouped, Grouped(..)
  , pattern Inline, Inline(..)
  , pattern Disabled, Disabled(..)
  , pattern Error, Error(..)
  , pattern Required, Required(..)
  , pattern Type, Type(..)
  , pattern One, pattern Two, pattern Three, pattern Four
  , pattern Five, pattern Six, pattern Seven, pattern Eight
  , pattern Nine, pattern Ten, pattern Eleven, pattern Twelve
  , pattern Thirteen, pattern Fourteen, pattern Fifteen, pattern Sixteen
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

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
    , widths :: Txt
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

instance HasProp As (Form ms) where
    type Prop As (Form ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a f = f { as = a }

instance HasProp Attributes (Form ms) where
    type Prop Attributes (Form ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as f = f { attributes = as }

instance HasProp Children (Form ms) where
    type Prop Children (Form ms) = [View ms]
    getProp _ = children
    setProp _ cs f = f { children = cs }

instance HasProp Classes (Form ms) where
    type Prop Classes (Form ms) = [Txt]
    getProp _ = classes
    setProp _ cs f = f { classes = cs }

instance HasProp Action (Form ms) where
    type Prop Action (Form ms) = Txt
    getProp _ = action
    setProp _ a f = f { action = a }

instance HasProp Error (Form ms) where
    type Prop Error (Form ms) = Bool
    getProp _ = error
    setProp _ e f = f { error = e }

instance HasProp Inverted (Form ms) where
    type Prop Inverted (Form ms) = Bool
    getProp _ = inverted
    setProp _ i f = f { inverted = i }

instance HasProp Loading (Form ms) where
    type Prop Loading (Form ms) = Bool
    getProp _ = loading
    setProp _ l f = f { loading = l }

instance HasProp OnSubmit (Form ms) where
    type Prop OnSubmit (Form ms) = Ef ms IO ()
    getProp _ = onSubmit
    setProp _ os f = f { onSubmit = os }

instance HasProp Reply (Form ms) where
    type Prop Reply (Form ms) = Bool
    getProp _ = reply
    setProp _ r f = f { reply = r }

instance HasProp Size (Form ms) where
    type Prop Size (Form ms) = Txt
    getProp _ = size
    setProp _ sz f = f { size = sz }

instance HasProp Success (Form ms) where
    type Prop Success (Form ms) = Bool
    getProp _ = success
    setProp _ s f = f { success = s }

instance HasProp Unstackable (Form ms) where
    type Prop Unstackable (Form ms) = Bool
    getProp _ = unstackable
    setProp _ u f = f { unstackable = u }

instance HasProp Warning (Form ms) where
    type Prop Warning (Form ms) = Bool
    getProp _ = warning
    setProp _ w f = f { warning = w }

instance HasProp Widths (Form ms) where
    type Prop Widths (Form ms) = Txt
    getProp _ = widths
    setProp _ w f = f { widths = w }

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
    , widths :: Txt
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

instance HasProp As (Field ms) where
    type Prop As (Field ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a ff = ff { as = a }

instance HasProp Attributes (Field ms) where
    type Prop Attributes (Field ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as ff = ff { attributes = as }

instance HasProp Children (Field ms) where
    type Prop Children (Field ms) = [View ms]
    getProp _ = children
    setProp _ cs ff = ff { children = cs }

instance HasProp Classes (Field ms) where
    type Prop Classes (Field ms) = [Txt]
    getProp _ = classes
    setProp _ cs ff = ff { classes = cs }

instance HasProp Disabled (Field ms) where
    type Prop Disabled (Field ms) = Bool
    getProp _ = disabled
    setProp _ d ff = ff { disabled = d }

instance HasProp Error (Field ms) where
    type Prop Error (Field ms) = Bool
    getProp _ = error
    setProp _ e ff = ff { error = e }

instance HasProp Inline (Field ms) where
    type Prop Inline (Field ms) = Bool
    type Prop Inline (Field ms) = Bool
    getProp _ = inline
    setProp _ i ff = ff { inline = i }

instance HasProp Required (Field ms) where
    type Prop Required (Field ms) = Bool
    getProp _ = required
    setProp _ r ff = ff { required = r }

instance HasProp Type (Field ms) where
    type Prop Type (Field ms) = Txt
    getProp _ = _type
    setProp _ t ff = ff { _type = t }

instance HasProp Widths (Field ms) where
    type Prop Widths (Field ms) = Txt
    getProp _ = widths
    setProp _ w ff = ff { widths = w }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , grouped :: Bool
    , inline :: Bool
    , unstackable :: Bool
    , widths :: Txt
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

instance HasProp As (Group ms) where
    type Prop As (Group ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a fg = fg { as = a }

instance HasProp Attributes (Group ms) where
    type Prop Attributes (Group ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as fg = fg { attributes = as }

instance HasProp Children (Group ms) where
    type Prop Children (Group ms) = [View ms]
    getProp _ = children
    setProp _ cs fg = fg { children = cs }

instance HasProp Classes (Group ms) where
    type Prop Classes (Group ms) = [Txt]
    getProp _ = classes
    setProp _ cs fg = fg { classes = cs }

instance HasProp Grouped (Group ms) where
    type Prop Grouped (Group ms) = Bool
    getProp _ = grouped
    setProp _ g fg = fg { grouped = g }

instance HasProp Inline (Group ms) where
    type Prop Inline (Group ms) = Bool
    getProp _ = inline
    setProp _ i fg = fg { inline = i }

instance HasProp Unstackable (Group ms) where
    type Prop Unstackable (Group ms) = Bool
    getProp _ = unstackable
    setProp _ u fg = fg { unstackable = u }

instance HasProp Widths (Group ms) where
    type Prop Widths (Group ms) = Txt
    getProp _ = widths
    setProp _ w fg = fg { widths = w }
