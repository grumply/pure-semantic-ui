{-# LANGUAGE UndecidableInstances #-}
module Semantic.Elements.List
  ( module Properties
  , module Tools
  , List(..), pattern List
  , Content(..), pattern Content
  , Description(..), pattern Description
  , Header(..), pattern Header
  , Icon(..), pattern Icon
  , Item(..), pattern Item
  , Sublist(..), pattern Sublist
  , Keyed(..), pattern Keyed
  ) where

import GHC.Generics as G
import Pure.View hiding (horizontal,onClick,verticalAlign,disabled,active,color,name,Content,Description,Header,Value)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Animated, Animated(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Bulleted, Bulleted(..)
  , pattern Celled, Celled(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Divided, Divided(..)
  , pattern Floated, Floated(..)
  , pattern Horizontal, Horizontal(..)
  , pattern Inverted, Inverted(..)
  , pattern Link, Link(..)
  , pattern OnClick, OnClick(..)
  , pattern Ordered, Ordered(..)
  , pattern Relaxed, Relaxed(..)
  , pattern Selection, Selection(..)
  , pattern Size, Size(..)
  , pattern VerticalAlign, VerticalAlign(..)
  , pattern Bordered, Bordered(..)
  , pattern Circular, Circular(..)
  , pattern Color, Color(..)
  , pattern Corner, Corner(..)
  , pattern Disabled, Disabled(..)
  , pattern Fitted, Fitted(..)
  , pattern Flipped, Flipped(..)
  , pattern Loading, Loading(..)
  , pattern Name, Name(..)
  , pattern Rotated, Rotated(..)
  , pattern Active, Active(..)
  , pattern Value, Value(..)
  )

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data List ms = List_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , animated :: Bool
    , bulleted :: Bool
    , celled :: Bool
    , divided :: Bool
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , link :: Bool
    , onItemClick :: Item ms -> Ef ms IO ()
    , ordered :: Bool
    , relaxed :: Maybe Txt
    , selection :: Bool
    , size :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (List ms) where
    def = (G.to gdef) { as = Div }

pattern List :: VC ms => List ms -> View ms
pattern List l = View l

instance VC ms => Pure List ms where
    render List_ {..} =
        let
            children' =
                mapPures (\li@(Item_ {}) -> li { onClick = onClick li >> onItemClick li }) children

            cs =
                ( "ui"
                : size
                : animated # "animated"
                : bulleted # "bulleted"
                : celled # "celled"
                : divided # "divided"
                : horizontal # "horizontal"
                : inverted # "inverted"
                : link # "link"
                : ordered # "ordered"
                : selection # "selection"
                : may (<>> "relaxed") relaxed
                : floated # (floated <>> "floated")
                : verticalAlign # (verticalAlign <>> "aligned")
                : "list"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children'

instance HasProp Animated (List ms) where
    type Prop Animated (List ms) = Bool
    getProp _ = animated
    setProp _ anim l = l { animated = anim }

instance HasProp As (List ms) where
    type Prop As (List ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasProp Attributes (List ms) where
    type Prop Attributes (List ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs l = l { attributes = cs }

instance HasProp Bulleted (List ms) where
    type Prop Bulleted (List ms) = Bool
    getProp _ = bulleted
    setProp _ b l = l { bulleted = b }

instance HasProp Celled (List ms) where
    type Prop Celled (List ms) = Bool
    getProp _ = celled
    setProp _ c l = l { celled = c }

instance HasProp Children (List ms) where
    type Prop Children (List ms) = [View ms]
    getProp _ = children
    setProp _ cs l = l { children = cs }

instance HasProp Classes (List ms) where
    type Prop Classes (List ms) = [Txt]
    getProp _ = classes
    setProp _ cs l = l { classes = cs }

instance HasProp Divided (List ms) where
    type Prop Divided (List ms) = Bool
    getProp _ = divided
    setProp _ d l = l { divided = d }

instance HasProp OnClick (List ms) where
    type Prop OnClick (List ms) = Item ms -> Ef ms IO ()
    getProp _ = onItemClick
    setProp _ oc l = l { onItemClick = oc }

instance HasProp Floated (List ms) where
    type Prop Floated (List ms) = Txt
    getProp _ = floated
    setProp _ f l = l { floated = f }

instance HasProp Horizontal (List ms) where
    type Prop Horizontal (List ms) = Bool
    getProp _ = horizontal
    setProp _ h l = l { horizontal = h }

instance HasProp Inverted (List ms) where
    type Prop Inverted (List ms) = Bool
    getProp _ = inverted
    setProp _ i l = l { inverted = i }

instance HasProp Link (List ms) where
    type Prop Link (List ms) = Bool
    getProp _ = link
    setProp _ lnk l = l { link = lnk }

instance HasProp Ordered (List ms) where
    type Prop Ordered (List ms) = Bool
    getProp _ = ordered
    setProp _ o l = l { ordered = o }

instance HasProp Relaxed (List ms) where
    type Prop Relaxed (List ms) = Maybe Txt
    getProp _ = relaxed
    setProp _ r l = l { relaxed = r }

instance HasProp Selection (List ms) where
    type Prop Selection (List ms) = Bool
    getProp _ = selection
    setProp _ s l = l { selection = s }

instance HasProp Size (List ms) where
    type Prop Size (List ms) = Txt
    getProp _ = size
    setProp _ s l = l { size = s }

instance HasProp VerticalAlign (List ms) where
    type Prop VerticalAlign (List ms) = Txt
    getProp _ = verticalAlign
    setProp _ va l = l { verticalAlign = va }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , floated :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content lc = View lc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( floated # (floated <>> "floated")
                : verticalAlign # (verticalAlign <>> "aligned")
                : "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasProp As (Content ms) where
    type Prop As (Content ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f lc = lc { as = f }

instance HasProp Attributes (Content ms) where
    type Prop Attributes (Content ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs lc = lc { attributes = cs }

instance HasProp Children (Content ms) where
    type Prop Children (Content ms) = [View ms]
    getProp _ = children
    setProp _ cs lc = lc { children = cs }

instance HasProp Classes (Content ms) where
    type Prop Classes (Content ms) = [Txt]
    getProp _ = classes
    setProp _ cs lc = lc { classes = cs }

instance HasProp Floated (Content ms) where
    type Prop Floated (Content ms) = Txt
    getProp _ = floated
    setProp _ f lc = lc { floated = f }

instance HasProp VerticalAlign (Content ms) where
    type Prop VerticalAlign (Content ms) = Txt
    getProp _ = verticalAlign
    setProp _ va lc = lc { verticalAlign = va }

data Description ms = Description_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Description ms) where
    def = (G.to gdef) { as = Div }

pattern Description :: Description ms -> View ms
pattern Description ld = View ld

instance Pure Description ms where
    render Description_ {..} =
        as ( ClassList (classes ++ [ "description" ]) : attributes ) children

instance HasProp As (Description ms) where
    type Prop As (Description ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f ld = ld { as = f }

instance HasProp Attributes (Description ms) where
    type Prop Attributes (Description ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs ld = ld { attributes = cs }

instance HasProp Children (Description ms) where
    type Prop Children (Description ms) = [View ms]
    getProp _ = children
    setProp _ cs ld = ld { children = cs }

instance HasProp Classes (Description ms) where
    type Prop Classes (Description ms) = [Txt]
    getProp _ = classes
    setProp _ cs ld = ld { classes = cs }

data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header lh = View lh

instance Pure Header ms where
    render Header_ {..} =
        as ( ClassList ( "header" : classes ) : attributes ) children

instance HasProp As (Header ms) where
    type Prop As (Header ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f lh = lh { as = f }

instance HasProp Attributes (Header ms) where
    type Prop Attributes (Header ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs lh = lh { attributes = cs }

instance HasProp Children (Header ms) where
    type Prop Children (Header ms) = [View ms]
    getProp _ = children
    setProp _ cs lh = lh { children = cs }

instance HasProp Classes (Header ms) where
    type Prop Classes (Header ms) = [Txt]
    getProp _ = classes
    setProp _ cs lh = lh { classes = cs }

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
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (Icon ms) where
    def = (G.to gdef) { as = I }

pattern Icon :: Icon ms -> View ms
pattern Icon li = View li

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
                : verticalAlign # (verticalAlign <>> "aligned")
                : "icon"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                []

instance HasProp As (Icon ms) where
    type Prop As (Icon ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f li = li { as = f }

instance HasProp Attributes (Icon ms) where
    type Prop Attributes (Icon ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs li = li { attributes = cs }

instance HasProp Bordered (Icon ms) where
    type Prop Bordered (Icon ms) = Bool
    getProp _ = bordered
    setProp _ b li = li { bordered = b }

instance HasProp Circular (Icon ms) where
    type Prop Circular (Icon ms) = Bool
    getProp _ = circular
    setProp _ c li = li { circular = c }

instance HasProp Name (Icon ms) where
    type Prop Name (Icon ms) = Txt
    getProp _ = name
    setProp _ n li = li { name = n }

instance HasProp Classes (Icon ms) where
    type Prop Classes (Icon ms) = [Txt]
    getProp _ = classes
    setProp _ cs li = li { classes = cs }

instance HasProp Color (Icon ms) where
    type Prop Color (Icon ms) = Txt
    getProp _ = color
    setProp _ c li = li { color = c }

instance HasProp Corner (Icon ms) where
    type Prop Corner (Icon ms) = Bool
    getProp _ = corner
    setProp _ c li = li { corner = c }

instance HasProp Disabled (Icon ms) where
    type Prop Disabled (Icon ms) = Bool
    getProp _ = disabled
    setProp _ d li = li { disabled = d }

instance HasProp Fitted (Icon ms) where
    type Prop Fitted (Icon ms) = Bool
    getProp _ = fitted
    setProp _ f li = li { fitted = f }

instance HasProp Flipped (Icon ms) where
    type Prop Flipped (Icon ms) = Txt
    getProp _ = flipped
    setProp _ f li = li { flipped = f }

instance HasProp Inverted (Icon ms) where
    type Prop Inverted (Icon ms) = Bool
    getProp _ = inverted
    setProp _ i li = li { inverted = i }

instance HasProp Link (Icon ms) where
    type Prop Link (Icon ms) = Bool
    getProp _ = link
    setProp _ l li = li { link = l }

instance HasProp Loading (Icon ms) where
    type Prop Loading (Icon ms) = Bool
    getProp _ = loading
    setProp _ l li = li { loading = l }

instance HasProp Rotated (Icon ms) where
    type Prop Rotated (Icon ms) = Txt
    getProp _ = rotated
    setProp _ r li = li { rotated = r }

instance HasProp Size (Icon ms) where
    type Prop Size (Icon ms) = Txt
    getProp _ = size
    setProp _ s li = li { size = s }

instance HasProp VerticalAlign (Icon ms) where
    type Prop VerticalAlign (Icon ms) = Txt
    getProp _ = verticalAlign
    setProp _ va li = li { verticalAlign = va }

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , disabled :: Bool
    , onClick :: Ef ms IO ()
    , value :: Txt
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Item ms -> View ms
pattern Item li = View li

instance Pure Item ms where
    render Item_ {..} =
        let
            li =
                case as [] [] of
                    Li _ _ -> True
                    _      -> False
            cs =
                ( active # "active"
                : disabled # "disabled"
                : (not li) # "item"
                : classes
                )

            valueProp = li ? HTML.Value value $ Prop "data-value" value
        in
            as
                ( mergeClasses $ HTML.onClick onClick
                : valueProp
                : ClassList cs
                : Role "listitem"
                : attributes
                )
                children

instance HasProp Active (Item ms) where
    type Prop Active (Item ms) = Bool
    getProp _ = active
    setProp _ a li = li { active = a }

instance HasProp As (Item ms) where
    type Prop As (Item ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f li = li { as = f }

instance HasProp Attributes (Item ms) where
    type Prop Attributes (Item ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs li = li { attributes = cs }

instance HasProp Children (Item ms) where
    type Prop Children (Item ms) = [View ms]
    getProp _ = children
    setProp _ cs li = li { children = cs }

instance HasProp Classes (Item ms) where
    type Prop Classes (Item ms) = [Txt]
    getProp _ = classes
    setProp _ cs li = li { classes = cs }

instance HasProp OnClick (Item ms) where
    type Prop OnClick (Item ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc li = li { onClick = oc }

instance HasProp Disabled (Item ms) where
    type Prop Disabled (Item ms) = Bool
    getProp _ = disabled
    setProp _ d li = li { disabled = d }

instance HasProp Value (Item ms) where
    type Prop Value (Item ms) = Txt
    getProp _ = value
    setProp _ v li = li { value = v }

data Sublist ms = Sublist_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Sublist ms) where
    def = (G.to gdef) { as = Div }

pattern Sublist :: Sublist ms -> View ms
pattern Sublist ll = View ll

instance Pure Sublist ms where
    render Sublist_ {..} =
        let
            proxy =
                case as [] [] of
                    Ul _ _ -> False
                    Ol _ _ -> False
                    _      -> True

        in
            as
                ( ClassList ( proxy # "list" : classes )
                : attributes
                )
                children

instance HasProp As (Sublist ms) where
    type Prop As (Sublist ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ f ll = ll { as = f }

instance HasProp Attributes (Sublist ms) where
    type Prop Attributes (Sublist ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs ll = ll { attributes = cs }

instance HasProp Children (Sublist ms) where
    type Prop Children (Sublist ms) = [View ms]
    getProp _ = children
    setProp _ cs ll = ll { children = cs }

instance HasProp Classes (Sublist ms) where
    type Prop Classes (Sublist ms) = [Txt]
    getProp _ = classes
    setProp _ cs ll = ll { classes = cs }

data Keyed ms = Keyed_
    { as :: [Feature ms] -> [(Int,View ms)] -> View ms
    , attributes :: [Feature ms]
    , children :: [(Int,View ms)]
    , classes :: [Txt]
    , animated :: Bool
    , bulleted :: Bool
    , celled :: Bool
    , divided :: Bool
    , floated :: Txt
    , horizontal :: Bool
    , inverted :: Bool
    , link :: Bool
    , onItemClick :: (Int,Item ms) -> Ef ms IO ()
    , ordered :: Bool
    , relaxed :: Maybe Txt
    , selection :: Bool
    , size :: Txt
    , verticalAlign :: Txt
    } deriving (Generic)

instance Default (Keyed ms) where
    def = (G.to gdef) { as = list Div }

pattern Keyed :: VC ms => Keyed ms -> View ms
pattern Keyed l = View l

instance VC ms => Pure Keyed ms where
    render Keyed_ {..} =
        let
            children' =
                flip map children $ \(n,c) ->
                    case c of
                        View li@Item_ {} -> (n,View li { onClick = onClick li >> onItemClick (n,li) })
                        _                    -> (n,c)

            cs =
                ( "ui"
                : size
                : animated # "animated"
                : bulleted # "bulleted"
                : celled # "celled"
                : divided # "divided"
                : horizontal # "horizontal"
                : inverted # "inverted"
                : link # "link"
                : ordered # "ordered"
                : selection # "selection"
                : may (<>> "relaxed") relaxed
                : floated # (floated <>> "floated")
                : verticalAlign # (verticalAlign <>> "aligned")
                : "list"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children'

instance HasProp Animated (Keyed ms) where
    type Prop Animated (Keyed ms) = Bool
    getProp _ = animated
    setProp _ anim l = l { animated = anim }

instance HasProp As (Keyed ms) where
    type Prop As (Keyed ms) = [Feature ms] -> [(Int,View ms)] -> View ms
    getProp _ = as
    setProp _ f l = l { as = f }

instance HasProp Attributes (Keyed ms) where
    type Prop Attributes (Keyed ms) = [Feature ms]
    getProp _ = attributes
    setProp _ cs l = l { attributes = cs }

instance HasProp Bulleted (Keyed ms) where
    type Prop Bulleted (Keyed ms) = Bool
    getProp _ = bulleted
    setProp _ b l = l { bulleted = b }

instance HasProp Celled (Keyed ms) where
    type Prop Celled (Keyed ms) = Bool
    getProp _ = celled
    setProp _ c l = l { celled = c }

instance HasProp Children (Keyed ms) where
    type Prop Children (Keyed ms) = [(Int,View ms)]
    getProp _ = children
    setProp _ cs l = l { children = cs }

instance HasProp Classes (Keyed ms) where
    type Prop Classes (Keyed ms) = [Txt]
    getProp _ = classes
    setProp _ cs l = l { classes = cs }

instance HasProp Divided (Keyed ms) where
    type Prop Divided (Keyed ms) = Bool
    getProp _ = divided
    setProp _ d l = l { divided = d }

instance HasProp OnClick (Keyed ms) where
    type Prop OnClick (Keyed ms) = (Int,Item ms) -> Ef ms IO ()
    getProp _ = onItemClick
    setProp _ oc l = l { onItemClick = oc }

instance HasProp Floated (Keyed ms) where
    type Prop Floated (Keyed ms) = Txt
    getProp _ = floated
    setProp _ f l = l { floated = f }

instance HasProp Horizontal (Keyed ms) where
    type Prop Horizontal (Keyed ms) = Bool
    getProp _ = horizontal
    setProp _ h l = l { horizontal = h }

instance HasProp Inverted (Keyed ms) where
    type Prop Inverted (Keyed ms) = Bool
    getProp _ = inverted
    setProp _ i l = l { inverted = i }

instance HasProp Link (Keyed ms) where
    type Prop Link (Keyed ms) = Bool
    getProp _ = link
    setProp _ lnk l = l { link = lnk }

instance HasProp Ordered (Keyed ms) where
    type Prop Ordered (Keyed ms) = Bool
    getProp _ = ordered
    setProp _ o l = l { ordered = o }

instance HasProp Relaxed (Keyed ms) where
    type Prop Relaxed (Keyed ms) = Maybe Txt
    getProp _ = relaxed
    setProp _ r l = l { relaxed = r }

instance HasProp Selection (Keyed ms) where
    type Prop Selection (Keyed ms) = Bool
    getProp _ = selection
    setProp _ s l = l { selection = s }

instance HasProp Size (Keyed ms) where
    type Prop Size (Keyed ms) = Txt
    getProp _ = size
    setProp _ s l = l { size = s }

instance HasProp VerticalAlign (Keyed ms) where
    type Prop VerticalAlign (Keyed ms) = Txt
    getProp _ = verticalAlign
    setProp _ va l = l { verticalAlign = va }
