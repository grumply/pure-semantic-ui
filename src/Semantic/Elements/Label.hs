module Semantic.Elements.Label where

import GHC.Generics as G
import Pure.View as View hiding (active,color,empty,horizontal,onClick)
import qualified Pure.View as HTML

import Semantic.Utils

import Semantic.Elements.Image (Image(..))

import Semantic.Properties as Properties
  ( HasActiveProp(..), pattern Active
  , HasAsProp(..), pattern As
  , HasAttachedProp(..), pattern Attached
  , HasAttributesProp(..), pattern Attributes
  , HasBasicProp(..), pattern Basic
  , HasChildrenProp(..), pattern Children
  , HasCircularProp(..), pattern Circular
  , HasClassesProp(..), pattern Classes
  , HasColorProp(..), pattern Color
  , HasCornerProp(..), pattern Corner
  , HasEmptyProp(..), pattern Empty
  , HasFloatingProp(..), pattern Floating
  , HasHorizontalProp(..), pattern Horizontal
  , HasOnClickProp(..), pattern OnClick
  , HasPointingProp(..), pattern Pointing
  , HasRibbonProp(..), pattern Ribbon
  , HasSizeProp(..), pattern Size
  , HasTagProp(..), pattern Tag
  )

data Label ms = Label_
    { as :: [Feature ms] -> [View ms] -> View ms
    , active :: Bool
    , attached :: Txt
    , attributes :: [Feature ms]
    , basic :: Bool
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , corner :: Maybe Txt
    , empty :: Bool
    , floating :: Bool
    , horizontal :: Bool
    , onClick :: Ef ms IO ()
    , pointing :: Maybe Txt
    , ribbon :: Maybe Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (Label ms) where
    def = (G.to gdef) { as = Div }

pattern Label :: Label ms -> View ms
pattern Label l = View l

instance Pure Label ms where
    render Label_ {..} =
        let
            pointingClass =
                -- note the careful class ordering
                ($ "pointing") $ flip (maybe (const nil)) pointing $ \case
                    (oneEq "left" "right"  -> Just lr) -> (lr <<>>)
                    (oneEq "above" "below" -> Just ab) -> (<<>> ab)
                    "" -> id
                    _  -> const nil

            hasImage =
                foldPures (\(Image_ {}) -> const True) False children

            cs =
                ( "ui"
                : color
                : pointingClass
                : size
                : active # "active"
                : basic # "basic"
                : circular # "circular"
                : empty # "empty"
                : floating # "floating"
                : horizontal # "horizontal"
                : hasImage # "image"
                : tag # "tag"
                : corner # "corner"
                : ribbon # "ribbon"
                : attached # "attached"
                : "label"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : onClick # HTML.onClick onClick
                : attributes
                )
                children

instance HasActiveProp (Label ms) where
    getActive = active
    setActive a l = l { active = a }

instance HasAsProp (Label ms) where
    type AsProp (Label ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f l = l { as = f }

instance HasAttachedProp (Label ms) where
    type AttachedProp (Label ms) = Txt
    getAttached = attached
    setAttached attach l = l { attached = attach }

instance HasAttributesProp (Label ms) where
    type Attribute (Label ms) = Feature ms
    getAttributes = attributes
    setAttributes cs l = l { attributes = cs }

instance HasBasicProp (Label ms) where
    getBasic = basic
    setBasic b l = l { basic = b }

instance HasChildrenProp (Label ms) where
    type Child (Label ms) = View ms
    getChildren = children
    setChildren cs l = l { children = cs }

instance HasCircularProp (Label ms) where
    getCircular = circular
    setCircular c l = l { circular = c }

instance HasClassesProp (Label ms) where
    getClasses = classes
    setClasses cs l = l { classes = cs }

instance HasOnClickProp (Label ms) where
    type OnClickProp (Label ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc l = l { onClick = oc }

instance HasColorProp (Label ms) where
    getColor = color
    setColor c l = l { color = c }

instance HasCornerProp (Label ms) where
    type CornerProp (Label ms) = Maybe Txt
    getCorner = corner
    setCorner c l = l { corner = c }

instance HasEmptyProp (Label ms) where
    getEmpty = empty
    setEmpty e l = l { empty = e }

instance HasFloatingProp (Label ms) where
    getFloating = floating
    setFloating f l = l { floating = f }

instance HasHorizontalProp (Label ms) where
    getHorizontal = horizontal
    setHorizontal h l = l { horizontal = h }

instance HasPointingProp (Label ms) where
    getPointing = pointing
    setPointing p l = l { pointing = p }

instance HasRibbonProp (Label ms) where
    getRibbon = ribbon
    setRibbon r l = l { ribbon = r }

instance HasSizeProp (Label ms) where
    getSize = size
    setSize s l = l { size = s }

instance HasTagProp (Label ms) where
    getTag = tag
    setTag t l = l { tag = t }

data Detail ms = Detail_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Detail ms) where
    def = G.to gdef

pattern Detail :: Detail ms -> View ms
pattern Detail ld = View ld

instance Pure Detail ms where
    render Detail_ {..} =
        as
            ( ClassList ("detail" : classes)
            : attributes
            )
            children

instance HasAsProp (Detail ms) where
    type AsProp (Detail ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f ld = ld { as = f }

instance HasAttributesProp (Detail ms) where
    type Attribute (Detail ms) = Feature ms
    getAttributes = attributes
    setAttributes cs ld = ld { attributes = cs }

instance HasChildrenProp (Detail ms) where
    type Child (Detail ms) = View ms
    getChildren = children
    setChildren cs ld = ld { children = cs }

instance HasClassesProp (Detail ms) where
    getClasses = classes
    setClasses cs ld = ld { classes = cs }

data Group ms = Group_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , circular :: Bool
    , classes :: [Txt]
    , color :: Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (Group ms) where
    def = (G.to gdef) { as = Div }

pattern Group :: Group ms -> View ms
pattern Group lg = View lg

instance Pure Group ms where
    render Group_ {..} =
        let
            cs =
                ( "ui"
                : color
                : size
                : circular # "circular"
                : tag # "tag"
                : "labels"
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
    setAs f lg = lg { as = f }

instance HasAttributesProp (Group ms) where
    type Attribute (Group ms) = Feature ms
    getAttributes = attributes
    setAttributes cs lg = lg { attributes = cs }

instance HasChildrenProp (Group ms) where
    type Child (Group ms) = View ms
    getChildren = children
    setChildren cs lg = lg { children = cs }

instance HasCircularProp (Group ms) where
    getCircular = circular
    setCircular c lg = lg { circular = c }

instance HasClassesProp (Group ms) where
    getClasses = classes
    setClasses cs lg = lg { classes = cs }

instance HasColorProp (Group ms) where
    getColor = color
    setColor c lg = lg { color = c }

instance HasSizeProp (Group ms) where
    getSize = size
    setSize s lg = lg { size = s }

instance HasTagProp (Group ms) where
    getTag = tag
    setTag t lg = lg { tag = t }
