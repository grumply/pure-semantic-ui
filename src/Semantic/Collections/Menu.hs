{-# LANGUAGE UndecidableInstances #-}
module Semantic.Collections.Menu
  ( module Properties
  , module Tools
  , Menu(..), pattern Menu
  , Header(..), pattern Header
  , Item(..), pattern Item
  , Submenu (..), pattern Submenu
  ) where

import GHC.Generics as G
import Pure.View hiding (active,color,fixed,onClick,text,vertical,widths,position,disabled,Menu,Header)

import Semantic.Utils

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasAttachedProp(..), pattern Attached
  , HasBorderlessProp(..), pattern Borderless
  , HasColorProp(..), pattern Color
  , HasCompactProp(..), pattern Compact
  , HasFixedProp(..), pattern Fixed
  , HasFloatedProp(..), pattern Floated
  , HasFluidProp(..), pattern Fluid
  , HasInvertedProp(..), pattern Inverted
  , HasIsIconProp(..), pattern IsIcon
  , HasIsTextProp(..), pattern IsText
  , HasOnClickProp(..), pattern OnClick
  , HasPaginationProp(..), pattern Pagination
  , HasPointingProp(..), pattern Pointing
  , HasSecondaryProp(..), pattern Secondary
  , HasSizeProp(..), pattern Size
  , HasStackableProp(..), pattern Stackable
  , HasTabularProp(..), pattern Tabular
  , HasVerticalProp(..), pattern Vertical
  , HasWidthsProp(..), pattern Widths
  , HasActiveProp(..), pattern Active
  , HasDisabledProp(..), pattern Disabled
  , HasFittedProp(..), pattern Fitted
  , HasIndexProp(..), pattern Index
  , HasIsHeaderProp(..), pattern IsHeader
  , HasLinkProp(..), pattern Link
  , HasPositionProp(..), pattern Position
  )

import Semantic.Elements.Icon

data Menu ms = Menu_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , borderless :: Bool
    , color :: Txt
    , compact :: Bool
    , fixed :: Txt
    , floated :: Maybe Txt
    , fluid :: Bool
    , icon :: Maybe Txt
    , inverted :: Bool
    , onItemClick :: Item ms -> Ef ms IO ()
    , pagination :: Bool
    , pointing :: Bool
    , secondary :: Bool
    , size :: Txt
    , stackable :: Bool
    , tabular :: Maybe Txt
    , text :: Bool
    , vertical :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (Menu ms) where
    def = (G.to gdef) { as = Div }

pattern Menu :: VC ms => Menu ms -> View ms
pattern Menu m = View m

instance VC ms => Pure Menu ms where
    render Menu_ {..} =
        let
            children' =
                mapPures (\mi@(Item_ {}) -> mi { onClick = onClick mi >> onItemClick mi }) children

            cs =
                ( "ui"
                : color
                : size
                : borderless # "borderless"
                : compact # "compact"
                : fluid # "fluid"
                : inverted # "inverted"
                : pagination # "pagination"
                : pointing # "pointing"
                : secondary # "secondary"
                : stackable # "stackable"
                : text # "text"
                : vertical # "vertical"
                : may (<>> "attached") attached
                : may (<>> "floated") floated
                : may (<>> "icon") icon
                : may (<>> "tabular") tabular
                : fixed # (fixed <>> "fixed")
                : widthProp widths "item" def
                : "menu"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children'

instance HasAsProp (Menu ms) where
    type AsProp (Menu ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a m = m { as = a }

instance HasAttributesProp (Menu ms) where
    type Attribute (Menu ms) = Feature ms
    getAttributes = attributes
    setAttributes as m = m { attributes = as }

instance HasChildrenProp (Menu ms) where
    type Child (Menu ms) = View ms
    getChildren = children
    setChildren cs m = m { children = cs }

instance HasClassesProp (Menu ms) where
    getClasses = classes
    setClasses cs m = m { classes = cs }

instance HasAttachedProp (Menu ms) where
    type AttachedProp (Menu ms) = Maybe Txt
    getAttached = attached
    setAttached a m = m { attached = a }

instance HasBorderlessProp (Menu ms) where
    getBorderless = borderless
    setBorderless b m = m { borderless = b }

instance HasColorProp (Menu ms) where
    getColor = color
    setColor c m = m { color = c }

instance HasCompactProp (Menu ms) where
    getCompact = compact
    setCompact c m = m { compact = c }

instance HasFixedProp (Menu ms) where
    getFixed = fixed
    setFixed f m = m { fixed = f }

instance HasFloatedProp (Menu ms) where
    type FloatedProp (Menu ms) = Maybe Txt
    getFloated = floated
    setFloated f m = m { floated = f }

instance HasFluidProp (Menu ms) where
    getFluid = fluid
    setFluid f m = m { fluid = f }

instance HasIsIconProp (Menu ms) where
    getIsIcon = icon
    setIsIcon i m = m { icon = i }

instance HasInvertedProp (Menu ms) where
    getInverted = inverted
    setInverted i m = m { inverted = i }

instance HasOnClickProp (Menu ms) where
    type OnClickProp (Menu ms) = Item ms -> Ef ms IO ()
    getOnClick = onItemClick
    setOnClick oc m = m { onItemClick = oc }

instance HasPaginationProp (Menu ms) where
    getPagination = pagination
    setPagination p m = m { pagination = p }

instance HasPointingProp (Menu ms) where
    type PointingProp (Menu ms) = Bool
    getPointing = pointing
    setPointing p m = m { pointing = p }

instance HasSecondaryProp (Menu ms) where
    getSecondary = secondary
    setSecondary s m = m { secondary = s }

instance HasSizeProp (Menu ms) where
    getSize = size
    setSize s m = m { size = s }

instance HasStackableProp (Menu ms) where
    type StackableProp (Menu ms) = Bool
    getStackable = stackable
    setStackable s m = m { stackable = s }

instance HasTabularProp (Menu ms) where
    getTabular = tabular
    setTabular t m = m { tabular = t }

instance HasIsTextProp (Menu ms) where
    getIsText = text
    setIsText t m = m { text = t }

instance HasVerticalProp (Menu ms) where
    getVertical = vertical
    setVertical v m = m { vertical = v }

instance HasWidthsProp (Menu ms) where
    getWidths = widths
    setWidths w m = m { widths = w }
data Header ms = Header_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Header ms) where
    def = (G.to gdef) { as = Div }

pattern Header :: Header ms -> View ms
pattern Header mh = View mh

instance Pure Header ms where
    render Header_ {..} =
        let
            cs =
                ( "header"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Header ms) where
    type AsProp (Header ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mh = mh { as = a }

instance HasAttributesProp (Header ms) where
    type Attribute (Header ms) = Feature ms
    getAttributes = attributes
    setAttributes as mh = mh { attributes = as }

instance HasChildrenProp (Header ms) where
    type Child (Header ms) = View ms
    getChildren = children
    setChildren cs mh = mh { children = cs }

instance HasClassesProp (Header ms) where
    getClasses = classes
    setClasses cs mh = mh { classes = cs }

data Item ms = Item_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , color :: Txt
    , disabled :: Bool
    , fitted :: Maybe Txt
    , header :: Bool
    , index :: Int
    , link :: Bool
    , onClick :: Ef ms IO ()
    , position :: Txt
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Div }

pattern Item :: Item ms -> View ms
pattern Item mi = View mi

instance Pure Item ms where
    render Item_ {..} =
        let
            e = onClick ? A $ as

            icon =
                case children of
                    [ Icon i ] -> True
                    _          -> False

            cs =
                ( color
                : position
                : active # "active"
                : disabled # "disabled"
                : icon # "icon"
                : header # "header"
                : link # "link"
                : may ("fitted" <<>>) fitted
                : "item"
                : classes
                )

        in
            e
                ( mergeClasses $ ClassList cs
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasAsProp (Item ms) where
    type AsProp (Item ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mi = mi { as = a }

instance HasAttributesProp (Item ms) where
    type Attribute (Item ms) = Feature ms
    getAttributes = attributes
    setAttributes as mi = mi { attributes = as }

instance HasChildrenProp (Item ms) where
    type Child (Item ms) = View ms
    getChildren = children
    setChildren cs mi = mi { children = cs }

instance HasClassesProp (Item ms) where
    getClasses = classes
    setClasses cs mi = mi { classes = cs }

instance HasActiveProp (Item ms) where
    getActive = active
    setActive a mi = mi { active = a }

instance HasColorProp (Item ms) where
    getColor = color
    setColor c mi = mi { color = c }

instance HasDisabledProp (Item ms) where
    getDisabled = disabled
    setDisabled d mi = mi { disabled = d }

instance HasFittedProp (Item ms) where
    type FittedProp (Item ms) = Maybe Txt
    getFitted = fitted
    setFitted f mi = mi { fitted = f }

instance HasIsHeaderProp (Item ms) where
    getIsHeader = header
    setIsHeader h mi = mi { header = h }

instance HasIndexProp (Item ms) where
    getIndex = index
    setIndex i mi = mi { index = i }

instance HasLinkProp (Item ms) where
    getLink = link
    setLink l mi = mi { link = l }

instance HasOnClickProp (Item ms) where
    type OnClickProp (Item ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc mi = mi { onClick = oc }

instance HasPositionProp (Item ms) where
    getPosition = position
    setPosition p mi = mi { position = p }


data Submenu ms = Submenu_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , position :: Txt
    } deriving (Generic)

instance Default (Submenu ms) where
    def = (G.to gdef) { as = Div }

pattern Submenu :: Submenu ms -> View ms
pattern Submenu mm = View mm

instance Pure Submenu ms where
    render Submenu_ {..} =
        let
            cs =
                ( position
                : "menu"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Submenu ms) where
    type AsProp (Submenu ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mm = mm { as = a }

instance HasAttributesProp (Submenu ms) where
    type Attribute (Submenu ms) = Feature ms
    getAttributes = attributes
    setAttributes as mm = mm { attributes = as }

instance HasChildrenProp (Submenu ms) where
    type Child (Submenu ms) = View ms
    getChildren = children
    setChildren cs mm = mm { children = cs }

instance HasClassesProp (Submenu ms) where
    getClasses = classes
    setClasses cs mm = mm { classes = cs }

instance HasPositionProp (Submenu ms) where
    getPosition = position
    setPosition p mm = mm { position = p }
