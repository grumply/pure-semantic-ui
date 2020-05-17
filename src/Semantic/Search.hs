module Semantic.Search
  ( module Properties
  , module Tools
  , Search(..), pattern Search
  , Categorized(..), pattern Categorized
  , Result(..), pattern Result
  , Results(..), pattern Results
  ) where

import Pure hiding ((#),active,focus,size,name)

import Control.Arrow ((&&&))
import GHC.Generics as G

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern As, As(..)
  , pattern Aligned, Aligned(..)
  , pattern Category, Category(..)
  , pattern Fluid, Fluid(..)
  , pattern Focus, Focus(..)
  , pattern Loading, Loading(..)
  , pattern Open, Open(..)
  , pattern Size, Size(..)
  , pattern Active, Active(..)
  , pattern Name, Name(..)
  , pattern OnClick, OnClick(..)
  )

import Data.Function as Tools ((&))

{-
Approaching this differently than Semantic-UI-React. Instead of managing
everything internally, I want this to be a pure component that is maximally
extensible so that customized managed search components can be built on top
of it without too much work. The Semantic-UI-React/search component should
be implementable with this approach with this pure component as a core.

I will likely split managed search components off into their own library
similarly to semantic-ui-pure-forms.
-}

data Search = Search_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , aligned :: Txt
    , category :: Bool
    , fluid :: Bool
    , focus :: Bool
    , loading :: Bool
    , open :: Bool
    , size :: Txt
    } deriving (Generic)

instance Default Search where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Search :: Search -> Search
pattern Search s = s

instance Pure Search where
    view Search_ {..} =
        let
            cs = [ "ui"
                 , open # "active visible"
                 , size
                 , category # "category"
                 , focus # "focus"
                 , fluid # "fluid"
                 , loading # "loading"
                 , (aligned /= mempty) # (aligned <<>> "aligned")
                 , "search"
                 ]

        in as (features & Classes cs) children

instance HasProp As Search where
    type Prop As Search = Features -> [View] -> View
    getProp _ = as
    setProp _ f s = s { as = f }

instance HasFeatures Search where
    getFeatures = features
    setFeatures cs s = s { features = cs }

instance HasChildren Search where
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasProp Aligned Search where
    type Prop Aligned Search = Txt
    getProp _ = aligned
    setProp _ a s = s { aligned = a }

instance HasProp Category Search where
    type Prop Category Search = Bool
    getProp _ = category
    setProp _ c s = s { category = c }

instance HasProp Fluid Search where
    type Prop Fluid Search = Bool
    getProp _ = fluid
    setProp _ f s = s { fluid = f }

instance HasProp Focus Search where
    type Prop Focus Search = Bool
    getProp _ = focus
    setProp _ f s = s { focus = f }

instance HasProp Loading Search where
    type Prop Loading Search = Bool
    getProp _ = loading
    setProp _ l s = s { loading = l }

instance HasProp Open Search where
    type Prop Open Search = Bool
    getProp _ = open
    setProp _ o s = s { open = o }

instance HasProp Size Search where
    type Prop Size Search = Txt
    getProp _ = size
    setProp _ sz s = s { size = sz }

data Categorized = Categorized_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , name :: Txt
    } deriving (Generic)

instance Default Categorized where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
        }

pattern Categorized :: Categorized -> Categorized
pattern Categorized sc = sc

instance Pure Categorized where
    view sc@Categorized_ {..} =
        let
            cs =
                [ active # "active"
                , "category"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Categorized where
    type Prop As Categorized = Features -> [View] -> View
    getProp _ = as
    setProp _ f sc = sc { as = f }

instance HasFeatures Categorized where
    getFeatures = features
    setFeatures cs sc = sc { features = cs }

instance HasChildren Categorized where
    getChildren = children
    setChildren cs sc = sc { children = cs }

instance HasProp Active Categorized where
    type Prop Active Categorized = Bool
    getProp _ = active
    setProp _ a sc = sc { active = a }

instance HasProp Name Categorized where
    type Prop Name Categorized = Txt
    getProp _ = name
    setProp _ n sc = sc { name = n }

data Result = Result_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    } deriving (Generic)

instance Default Result where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Result :: Result -> Result
pattern Result sr = sr

instance Pure Result where
    view sr@Result_ {..} =
        let
            cs =
                [ active # "active"
                , "result"
                ]
        in
            as (features & Classes cs) children

instance HasProp As Result where
    type Prop As Result = Features -> [View] -> View
    getProp _ = as
    setProp _ f sr = sr { as = f }

instance HasFeatures Result where
    getFeatures = features
    setFeatures cs sr = sr { features = cs }

instance HasChildren Result where
    getChildren = children
    setChildren cs sr = sr { children = cs }

instance HasProp Active Result where
    type Prop Active Result = Bool
    getProp _ = active
    setProp _ a sr = sr { active = a }

data Results = Results_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    } deriving (Generic)

instance Default Results where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs }

pattern Results :: Results -> Results
pattern Results sr = sr

instance Pure Results where
    view Results_ {..} = as (features & Class "results transition") children

instance HasProp As Results where
    type Prop As Results = Features -> [View] -> View
    getProp _ = as
    setProp _ f sr = sr { as = f }

instance HasFeatures Results where
    getFeatures = features
    setFeatures cs sr = sr { features = cs }

instance HasChildren Results where
    getChildren = children
    setChildren cs sr = sr { children = cs }

