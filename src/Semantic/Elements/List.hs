{-# LANGUAGE UndecidableInstances #-}
module Semantic.Elements.List where

import GHC.Generics as G
import Pure.View hiding (horizontal,onClick,verticalAlign,disabled,active,color,name)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAnimatedProp(..), pattern Animated
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasBulletedProp(..), pattern Bulleted
  , HasCelledProp(..), pattern Celled
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasDividedProp(..), pattern Divided
  , HasFloatedProp(..), pattern Floated
  , HasHorizontalProp(..), pattern Horizontal
  , HasInvertedProp(..), pattern Inverted
  , HasLinkProp(..), pattern Link
  , HasOnClickProp(..), pattern OnClick
  , HasOrderedProp(..), pattern Ordered
  , HasRelaxedProp(..), pattern Relaxed
  , HasSelectionProp(..), pattern Selection
  , HasSizeProp(..), pattern Size
  , HasVerticalAlignProp(..), pattern VerticalAlign
  , HasBorderedProp(..), pattern Bordered
  , HasCircularProp(..), pattern Circular
  , HasColorProp(..), pattern Color
  , HasCornerProp(..), pattern Corner
  , HasDisabledProp(..), pattern Disabled
  , HasFittedProp(..), pattern Fitted
  , HasFlippedProp(..), pattern Flipped
  , HasLoadingProp(..), pattern Loading
  , HasNameProp(..), pattern Name
  , HasRotatedProp(..), pattern Rotated
  , HasActiveProp(..), pattern Active
  , HasValueProp(..), pattern Value
  )

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

instance HasAnimatedProp (List ms) where
    type AnimatedProp (List ms) = Bool
    getAnimated = animated
    setAnimated anim l = l { animated = anim }

instance HasAsProp (List ms) where
    type AsProp (List ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f l = l { as = f }

instance HasAttributesProp (List ms) where
    type Attribute (List ms) = Feature ms
    getAttributes = attributes
    setAttributes cs l = l { attributes = cs }

instance HasBulletedProp (List ms) where
    getBulleted = bulleted
    setBulleted b l = l { bulleted = b }

instance HasCelledProp (List ms) where
    getCelled = celled
    setCelled c l = l { celled = c }

instance HasChildrenProp (List ms) where
    type Child (List ms) = View ms
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasClassesProp (List ms) where
    getClasses = classes
    setClasses cs l = l { classes = cs }

instance HasDividedProp (List ms) where
    getDivided = divided
    setDivided d l = l { divided = d }

instance HasOnClickProp (List ms) where
    type OnClickProp (List ms) = Item ms -> Ef ms IO ()
    getOnClick = onItemClick
    setOnClick oc l = l { onItemClick = oc }

instance HasFloatedProp (List ms) where
    getFloated = floated
    setFloated f l = l { floated = f }

instance HasHorizontalProp (List ms) where
    getHorizontal = horizontal
    setHorizontal h l = l { horizontal = h }

instance HasInvertedProp (List ms) where
    getInverted = inverted
    setInverted i l = l { inverted = i }

instance HasLinkProp (List ms) where
    getLink = link
    setLink lnk l = l { link = lnk }

instance HasOrderedProp (List ms) where
    getOrdered = ordered
    setOrdered o l = l { ordered = o }

instance HasRelaxedProp (List ms) where
    getRelaxed = relaxed
    setRelaxed r l = l { relaxed = r }

instance HasSelectionProp (List ms) where
    getSelection = selection
    setSelection s l = l { selection = s }

instance HasSizeProp (List ms) where
    getSize = size
    setSize s l = l { size = s }

instance HasVerticalAlignProp (List ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va l = l { verticalAlign = va }

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

instance HasAsProp (Content ms) where
    type AsProp (Content ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f lc = lc { as = f }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes cs lc = lc { attributes = cs }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs lc = lc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs lc = lc { classes = cs }

instance HasFloatedProp (Content ms) where
    getFloated = floated
    setFloated f lc = lc { floated = f }

instance HasVerticalAlignProp (Content ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va lc = lc { verticalAlign = va }

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

instance HasAsProp (Description ms) where
    type AsProp (Description ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ld = ld { as = f }

instance HasAttributesProp (Description ms) where
    type Attribute (Description ms) = Feature ms
    getAttributes = attributes
    setAttributes cs ld = ld { attributes = cs }

instance HasChildrenProp (Description ms) where
    type Child (Description ms) = View ms
    getChildren = children
    setChildren cs ld = ld { children = cs }

instance HasClassesProp (Description ms) where
    getClasses = classes
    setClasses cs ld = ld { classes = cs }

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

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f lh = lh { as = f }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes cs lh = lh { attributes = cs }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs lh = lh { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs lh = lh { classes = cs }

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

instance HasAsProp (Icon ms) where
    type AsProp (Icon ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f li = li { as = f }

instance HasAttributesProp (Icon ms) where
    type Attribute (Icon ms) = Feature ms
    getAttributes = attributes
    setAttributes cs li = li { attributes = cs }

instance HasBorderedProp (Icon ms) where
    getBordered = bordered
    setBordered b li = li { bordered = b }

instance HasCircularProp (Icon ms) where
    getCircular = circular
    setCircular c li = li { circular = c }

instance HasNameProp (Icon ms) where
    getName = name
    setName n li = li { name = n }

instance HasClassesProp (Icon ms) where
    getClasses = classes
    setClasses cs li = li { classes = cs }

instance HasColorProp (Icon ms) where
    getColor = color
    setColor c li = li { color = c }

instance HasCornerProp (Icon ms) where
    type CornerProp (Icon ms) = Bool
    getCorner = corner
    setCorner c li = li { corner = c }

instance HasDisabledProp (Icon ms) where
    getDisabled = disabled
    setDisabled d li = li { disabled = d }

instance HasFittedProp (Icon ms) where
    getFitted = fitted
    setFitted f li = li { fitted = f }

instance HasFlippedProp (Icon ms) where
    getFlipped = flipped
    setFlipped f li = li { flipped = f }

instance HasInvertedProp (Icon ms) where
    getInverted = inverted
    setInverted i li = li { inverted = i }

instance HasLinkProp (Icon ms) where
    getLink = link
    setLink l li = li { link = l }

instance HasLoadingProp (Icon ms) where
    getLoading = loading
    setLoading l li = li { loading = l }

instance HasRotatedProp (Icon ms) where
    getRotated = rotated
    setRotated r li = li { rotated = r }

instance HasSizeProp (Icon ms) where
    getSize = size
    setSize s li = li { size = s }

instance HasVerticalAlignProp (Icon ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va li = li { verticalAlign = va }

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

instance HasActiveProp (Item ms) where
    getActive = active
    setActive a li = li { active = a }

instance HasAsProp (Item ms) where
    type AsProp (Item ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f li = li { as = f }

instance HasAttributesProp (Item ms) where
    type Attribute (Item ms) = Feature ms
    getAttributes = attributes
    setAttributes cs li = li { attributes = cs }

instance HasChildrenProp (Item ms) where
    type Child (Item ms) = View ms
    getChildren = children
    setChildren cs li = li { children = cs }

instance HasClassesProp (Item ms) where
    getClasses = classes
    setClasses cs li = li { classes = cs }

instance HasOnClickProp (Item ms) where
    type OnClickProp (Item ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc li = li { onClick = oc }

instance HasDisabledProp (Item ms) where
    getDisabled = disabled
    setDisabled d li = li { disabled = d }

instance HasValueProp (Item ms) where
    getValue = value
    setValue v li = li { value = v }

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

instance HasAsProp (Sublist ms) where
    type AsProp (Sublist ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ll = ll { as = f }

instance HasAttributesProp (Sublist ms) where
    type Attribute (Sublist ms) = Feature ms
    getAttributes = attributes
    setAttributes cs ll = ll { attributes = cs }

instance HasChildrenProp (Sublist ms) where
    type Child (Sublist ms) = View ms
    getChildren = children
    setChildren cs ll = ll { children = cs }

instance HasClassesProp (Sublist ms) where
    getClasses = classes
    setClasses cs ll = ll { classes = cs }

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

instance HasAnimatedProp (Keyed ms) where
    type AnimatedProp (Keyed ms) = Bool
    getAnimated = animated
    setAnimated anim l = l { animated = anim }

instance HasAsProp (Keyed ms) where
    type AsProp (Keyed ms) = [Feature ms] -> [(Int,View ms)] -> View ms
    getAs = as
    setAs f l = l { as = f }

instance HasAttributesProp (Keyed ms) where
    type Attribute (Keyed ms) = Feature ms
    getAttributes = attributes
    setAttributes cs l = l { attributes = cs }

instance HasBulletedProp (Keyed ms) where
    getBulleted = bulleted
    setBulleted b l = l { bulleted = b }

instance HasCelledProp (Keyed ms) where
    getCelled = celled
    setCelled c l = l { celled = c }

instance HasChildrenProp (Keyed ms) where
    type Child (Keyed ms) = (Int,View ms)
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasClassesProp (Keyed ms) where
    getClasses = classes
    setClasses cs l = l { classes = cs }

instance HasDividedProp (Keyed ms) where
    getDivided = divided
    setDivided d l = l { divided = d }

instance HasOnClickProp (Keyed ms) where
    type OnClickProp (Keyed ms) = (Int,Item ms) -> Ef ms IO ()
    getOnClick = onItemClick
    setOnClick oc l = l { onItemClick = oc }

instance HasFloatedProp (Keyed ms) where
    getFloated = floated
    setFloated f l = l { floated = f }

instance HasHorizontalProp (Keyed ms) where
    getHorizontal = horizontal
    setHorizontal h l = l { horizontal = h }

instance HasInvertedProp (Keyed ms) where
    getInverted = inverted
    setInverted i l = l { inverted = i }

instance HasLinkProp (Keyed ms) where
    getLink = link
    setLink lnk l = l { link = lnk }

instance HasOrderedProp (Keyed ms) where
    getOrdered = ordered
    setOrdered o l = l { ordered = o }

instance HasRelaxedProp (Keyed ms) where
    getRelaxed = relaxed
    setRelaxed r l = l { relaxed = r }

instance HasSelectionProp (Keyed ms) where
    getSelection = selection
    setSelection s l = l { selection = s }

instance HasSizeProp (Keyed ms) where
    getSize = size
    setSize s l = l { size = s }

instance HasVerticalAlignProp (Keyed ms) where
    getVerticalAlign = verticalAlign
    setVerticalAlign va l = l { verticalAlign = va }
