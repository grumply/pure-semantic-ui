module Semantic.Form
  ( module Properties
  , module Tools
  , Form(..), pattern Form
  , Field(..), pattern Field
  , Group(..), pattern Group
  ) where

import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML
import Pure.Data.Event

import Semantic.Utils
import Prelude hiding (error)

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
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

data Form = Form_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , action :: Txt
    , error :: Bool
    , inverted :: Bool
    , loading :: Bool
    , onSubmit :: IO ()
    , reply :: Bool
    , size :: Txt
    , success :: Bool
    , unstackable :: Bool
    , warning :: Bool
    , widths :: Txt
    } deriving (Generic)

instance Default Form where
    def = (G.to gdef) { as = \fs cs -> HTML.Form & Features fs & Children cs }

pattern Form :: Form -> Form
pattern Form f = f

instance Pure Form where
    view Form_ {..} =
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
                )
        in
            as
                : Prop "action" action
                : onSubmit # (On "submit" def { preventDef = True } (\_ -> return $ Just onSubmit))
                : attributes
                )
                children

instance HasProp As Form where
    type Prop As Form = Features -> [View] -> View
    getProp _ = as
    setProp _ a f = f { as = a }

instance HasFeatures Form where
    getFeatures = features
    setFeatures as f = f { features = as }

instance HasChildren Form where
    getChildren = children
    setChildren cs f = f { children = cs }

instance HasProp Action Form where
    type Prop Action Form = Txt
    getProp _ = action
    setProp _ a f = f { action = a }

instance HasProp Error Form where
    type Prop Error Form = Bool
    getProp _ = error
    setProp _ e f = f { error = e }

instance HasProp Inverted Form where
    type Prop Inverted Form = Bool
    getProp _ = inverted
    setProp _ i f = f { inverted = i }

instance HasProp Loading Form where
    type Prop Loading Form = Bool
    getProp _ = loading
    setProp _ l f = f { loading = l }

instance HasProp OnSubmit Form where
    type Prop OnSubmit Form = IO ()
    getProp _ = onSubmit
    setProp _ os f = f { onSubmit = os }

instance HasProp Reply Form where
    type Prop Reply Form = Bool
    getProp _ = reply
    setProp _ r f = f { reply = r }

instance HasProp Size Form where
    type Prop Size Form = Txt
    getProp _ = size
    setProp _ sz f = f { size = sz }

instance HasProp Success Form where
    type Prop Success Form = Bool
    getProp _ = success
    setProp _ s f = f { success = s }

instance HasProp Unstackable Form where
    type Prop Unstackable Form = Bool
    getProp _ = unstackable
    setProp _ u f = f { unstackable = u }

instance HasProp Warning Form where
    type Prop Warning Form = Bool
    getProp _ = warning
    setProp _ w f = f { warning = w }

instance HasProp Widths Form where
    type Prop Widths Form = Txt
    getProp _ = widths
    setProp _ w f = f { widths = w }

data Field = Field_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , disabled :: Bool
    , error :: Bool
    , inline :: Bool
    , required :: Bool
    , _type :: Txt
    , widths :: Txt
    } deriving (Generic)

instance Default Field where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Field :: Field -> Field
pattern Field ff = ff

instance Pure Field where
    view Field_ {..} =
        let
            cs =
                ( disabled # "disabled"
                : error # "error"
                : inline # "inline"
                : required # "required"
                : widthProp widths "wide" def
                : "field"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Field where
    type Prop As Field = Features -> [View] -> View
    getProp _ = as
    setProp _ a ff = ff { as = a }

instance HasFeatures Field where
    getFeatures = features
    setFeatures as ff = ff { features = as }

instance HasChildren Field where
    getChildren = children
    setChildren cs ff = ff { children = cs }

instance HasProp Disabled Field where
    type Prop Disabled Field = Bool
    getProp _ = disabled
    setProp _ d ff = ff { disabled = d }

instance HasProp Error Field where
    type Prop Error Field = Bool
    getProp _ = error
    setProp _ e ff = ff { error = e }

instance HasProp Inline Field where
    type Prop Inline Field = Bool
    type Prop Inline Field = Bool
    getProp _ = inline
    setProp _ i ff = ff { inline = i }

instance HasProp Required Field where
    type Prop Required Field = Bool
    getProp _ = required
    setProp _ r ff = ff { required = r }

instance HasProp Type Field where
    type Prop Type Field = Txt
    getProp _ = _type
    setProp _ t ff = ff { _type = t }

instance HasProp Widths Field where
    type Prop Widths Field = Txt
    getProp _ = widths
    setProp _ w ff = ff { widths = w }

data Group = Group_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , grouped :: Bool
    , inline :: Bool
    , unstackable :: Bool
    , widths :: Txt
    } deriving (Generic)

instance Default Group where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Group :: Group -> Group
pattern Group fg = fg

instance Pure Group where
    view Group_ {..} =
        let
            cs =
                ( grouped # "grouped"
                : inline # "inline"
                : unstackable # "unstackable"
                : widthProp widths def True
                : "fields"
                )
        in
            as
                : attributes
                )
                children

instance HasProp As Group where
    type Prop As Group = Features -> [View] -> View
    getProp _ = as
    setProp _ a fg = fg { as = a }

instance HasFeatures Group where
    getFeatures = features
    setFeatures as fg = fg { features = as }

instance HasChildren Group where
    getChildren = children
    setChildren cs fg = fg { children = cs }

instance HasProp Grouped Group where
    type Prop Grouped Group = Bool
    getProp _ = grouped
    setProp _ g fg = fg { grouped = g }

instance HasProp Inline Group where
    type Prop Inline Group = Bool
    getProp _ = inline
    setProp _ i fg = fg { inline = i }

instance HasProp Unstackable Group where
    type Prop Unstackable Group = Bool
    getProp _ = unstackable
    setProp _ u fg = fg { unstackable = u }

instance HasProp Widths Group where
    type Prop Widths Group = Txt
    getProp _ = widths
    setProp _ w fg = fg { widths = w }
