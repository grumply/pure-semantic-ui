{-# LANGUAGE UndecidableInstances #-}
module Semantic.Collections.Menu (module Semantic.Collections.Menu, module Export) where

import GHC.Generics as G
import Pure.View hiding (active,color,fixed,onClick,text,vertical,widths)

import Semantic.Utils

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
  )

import Semantic.Collections.Menu.MenuHeader as Export
import Semantic.Collections.Menu.MenuItem as Export
import Semantic.Collections.Menu.MenuMenu as Export

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
    , onItemClick :: MenuItem ms -> Ef ms IO ()
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
                mapPures (\mi@(MenuItem_ {}) -> mi { onClick = onClick mi >> onItemClick mi }) children
            
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
    type OnClickProp (Menu ms) = MenuItem ms -> Ef ms IO ()
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
