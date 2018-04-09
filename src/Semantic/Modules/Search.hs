module Semantic.Modules.Search
  ( module Properties
  , module Tools
  , Search(..), pattern Search
  , Categorized(..), pattern Categorized
  , Result(..), pattern Result
  , Results(..), pattern Results
  ) where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (onFocus,onBlur,name,active,Result,onClick)

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Aligned, Aligned(..)
  , pattern Category, Category(..)
  , pattern Fluid, Fluid(..)
  , pattern Focus, Focus(..)
  , pattern Loading, Loading(..)
  , pattern OnBlur, OnBlur(..)
  , pattern OnFocus, OnFocus(..)
  , pattern OnMouseDown, OnMouseDown(..)
  , pattern Open, Open(..)
  , pattern Size, Size(..)
  , pattern Active, Active(..)
  , pattern Name, Name(..)
  , pattern OnClick, OnClick(..)
  )

{-
Approaching this differently than Semantic-UI-React. Instead of managing
everything internally, I want this to be a pure component that is maximally
extensible so that customized managed search components can be built on top
of it without too much work. The Semantic-UI-React/search component should
be implementable with this approach with this pure component as a core.

I will likely split managed search components off into their own library
similarly to semantic-ui-pure-forms.
-}

data Search ms = Search_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , aligned :: Txt
    , category :: Bool
    , fluid :: Bool
    , focus :: Bool
    , loading :: Bool
    , onBlur :: Ef ms IO ()
    , onFocus :: Ef ms IO ()
    , onMouseDown :: Ef ms IO ()
    , open :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default (Search ms) where
    def = (G.to gdef) { as = Div }

pattern Search :: Search ms -> View ms
pattern Search s = View s

instance Pure Search ms where
    render Search_ {..} =
        let
            cs = ( "ui"
                 : open # "active visible"
                 : size
                 : category # "category"
                 : focus # "focus"
                 : fluid # "fluid"
                 : loading # "loading"
                 : aligned # "aligned"
                 : "search"
                 : classes
                 )

        in as
               ( mergeClasses $ ClassList cs
               : onMouseDown # On "mousedown" def (\_ -> return $ Just onMouseDown)
               : onFocus # On "focusin" def (\_ -> return $ Just onFocus)
               : onBlur # On "focusout" def (\_ -> return $ Just onBlur)
               : attributes
               )
               children

instance HasProp As (Search ms) where
    type Prop As (Search ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f s = s { as = f }

instance HasProp Attributes (Search ms) where
    type Prop Attributes (Search ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs s = s { attributes = cs }

instance HasProp Children (Search ms) where
    type Prop Children (Search ms) = [View ms]
    getProp _ = children
    setProp _ cs s = s { children = cs }

instance HasProp Classes (Search ms) where
    type Prop Classes (Search ms) = [Txt]
    getProp _ = classes
    setProp _ cs s = s { classes = cs }

instance HasProp Aligned (Search ms) where
    type Prop Aligned (Search ms) = Txt
    getProp _ = aligned
    setProp _ a s = s { aligned = a }

instance HasProp Category (Search ms) where
    type Prop Category (Search ms) = Bool
    getProp _ = category
    setProp _ c s = s { category = c }

instance HasProp Fluid (Search ms) where
    type Prop Fluid (Search ms) = Bool
    getProp _ = fluid
    setProp _ f s = s { fluid = f }

instance HasProp Focus (Search ms) where
    type Prop Focus (Search ms) = Bool
    getProp _ = focus
    setProp _ f s = s { focus = f }

instance HasProp Loading (Search ms) where
    type Prop Loading (Search ms) = Bool
    getProp _ = loading
    setProp _ l s = s { loading = l }

instance HasProp OnBlur (Search ms) where
    type Prop OnBlur (Search ms) = Ef ms IO ()
    getProp _ = onBlur
    setProp _ ob s = s { onBlur = ob }

instance HasProp OnFocus (Search ms) where
    type Prop OnFocus (Search ms) = Ef ms IO ()
    getProp _ = onFocus
    setProp _ onf s = s { onFocus = onf }

instance HasProp OnMouseDown (Search ms) where
    type Prop OnMouseDown (Search ms) = Ef ms IO ()
    getProp _ = onMouseDown
    setProp _ omd s = s { onMouseDown = omd }

instance HasProp Open (Search ms) where
    type Prop Open (Search ms) = Bool
    getProp _ = open
    setProp _ o s = s { open = o }

instance HasProp Size (Search ms) where
    type Prop Size (Search ms) = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

data Categorized ms = Categorized_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , name :: Txt
    } deriving (Generic)

instance Default (Categorized ms) where
    def = (G.to gdef)
        { as = Div
        }

pattern Categorized :: Categorized ms -> View ms
pattern Categorized sc = View sc

instance Pure Categorized ms where
    render sc@Categorized_ {..} =
        let
            cs =
                ( active # "active"
                : "category"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Categorized ms) where
    type Prop As (Categorized ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f sc = sc { as = f }

instance HasProp Attributes (Categorized ms) where
    type Prop Attributes (Categorized ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs sc = sc { attributes = cs }

instance HasProp Children (Categorized ms) where
    type Prop Children (Categorized ms) = [View ms]
    getProp _ = children
    setProp _ cs sc = sc { children = cs }

instance HasProp Classes (Categorized ms) where
    type Prop Classes (Categorized ms) = [Txt]
    getProp _ = classes
    setProp _ cs sc = sc { classes = cs }

instance HasProp Active (Categorized ms) where
    type Prop Active (Categorized ms) = Bool
    getProp _ = active
    setProp _ a sc = sc { active = a }

instance HasProp Name (Categorized ms) where
    type Prop Name (Categorized ms) = Txt
    getProp _ = name
    setProp _ n sc = sc { name = n }

data Result ms = Result_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (Result ms) where
    def = (G.to gdef) { as = Div }

pattern Result :: Result ms -> View ms
pattern Result sr = View sr

instance Pure Result ms where
    render sr@Result_ {..} =
        let
            cs =
                ( active # "active"
                : "result"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : onClick # On "click" def (\_ -> return $ Just onClick)
                : attributes
                )
                children

instance HasProp As (Result ms) where
    type Prop As (Result ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f sr = sr { as = f }

instance HasProp Attributes (Result ms) where
    type Prop Attributes (Result ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs sr = sr { attributes = cs }

instance HasProp Children (Result ms) where
    type Prop Children (Result ms) = [View ms]
    getProp _ = children
    setProp _ cs sr = sr { children = cs }

instance HasProp Classes (Result ms) where
    type Prop Classes (Result ms) = [Txt]
    getProp _ = classes
    setProp _ cs sr = sr { classes = cs }

instance HasProp Active (Result ms) where
    type Prop Active (Result ms) = Bool
    getProp _ = active
    setProp _ a sr = sr { active = a }

instance HasProp OnClick (Result ms) where
    type Prop OnClick (Result ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc sr = sr { onClick = oc }

data Results ms = Results_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Results ms) where
    def = (G.to gdef) { as = Div }

pattern Results :: Results ms -> View ms
pattern Results sr = View sr

instance Pure Results ms where
    render Results_ {..} =
        let
            cs =
                ( "results transition"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Results ms) where
    type Prop As (Results ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f sr = sr { as = f }

instance HasProp Attributes (Results ms) where
    type Prop Attributes (Results ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs sr = sr { attributes = cs }

instance HasProp Children (Results ms) where
    type Prop Children (Results ms) = [View ms]
    getProp _ = children
    setProp _ cs sr = sr { children = cs }

instance HasProp Classes (Results ms) where
    type Prop Classes (Results ms) = [Txt]
    getProp _ = classes
    setProp _ cs sr = sr { classes = cs }
