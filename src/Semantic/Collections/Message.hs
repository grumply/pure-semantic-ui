module Semantic.Collections.Message
  ( module Properties
  , module Tools
  , Message(..), pattern Message
  , Content(..), pattern Content
  , Header(..), pattern Header
  , Item(..), pattern Item
  , List(..), pattern List
  ) where

import GHC.Generics as G
import Pure.View hiding (color,hidden,visible,Name,Content,Header)

import Semantic.Utils

import Semantic.Elements.Icon

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasNameProp(..), pattern Name
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasAttachedProp(..), pattern Attached
  , HasColorProp(..), pattern Color
  , HasCompactProp(..), pattern Compact
  , HasErrorProp(..), pattern Error
  , HasFloatingProp(..), pattern Floating
  , HasHiddenProp(..), pattern Hidden
  , HasInfoProp(..), pattern Info
  , HasNegativeProp(..), pattern Negative
  , HasOnDismissProp(..), pattern OnDismiss
  , HasPositiveProp(..), pattern Positive
  , HasSizeProp(..), pattern Size
  , HasSuccessProp(..), pattern Success
  , HasVisibleProp(..), pattern Visible
  , HasWarningProp(..), pattern Warning
  )

import Prelude hiding (error)

data Message ms = Message_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , color :: Txt
    , compact :: Bool
    , error :: Bool
    , floating :: Bool
    , hidden :: Bool
    , info :: Bool
    , negative :: Bool
    , onDismiss :: Ef ms IO ()
    , positive :: Bool
    , size :: Txt
    , success :: Bool
    , visible :: Bool
    , warning :: Bool
    } deriving (Generic)

instance Default (Message ms) where
    def = (G.to gdef) { as = Div }

pattern Message :: Message ms -> View ms
pattern Message m = View m

instance Pure Message ms where
    render Message_ {..} =
        let
            icon = foldPures (\(Icon_ {}) -> const True) False children

            dismissIcon = onDismiss # (Icon $ def & Name "close" & Attributes [ On "click" def (\_ -> return $ Just onDismiss) ])

            cs =
                ( "ui"
                : color
                : size
                : compact # "compact"
                : error # "error"
                : floating # "floating"
                : hidden # "hidden"
                : icon # "icon"
                : info # "info"
                : negative # "negative"
                : positive # "positive"
                : success # "success"
                : visible # "visible"
                : warning # "warning"
                : may (<>> "attached") attached
                : "message"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                ( dismissIcon : children )

instance HasAsProp (Message ms) where
    type AsProp (Message ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a m = m { as = a }

instance HasAttributesProp (Message ms) where
    type Attribute (Message ms) = Feature ms
    getAttributes = attributes
    setAttributes as m = m { attributes = as }

instance HasChildrenProp (Message ms) where
    type Child (Message ms) = View ms
    getChildren = children
    setChildren cs m = m { children = cs }

instance HasClassesProp (Message ms) where
    getClasses = classes
    setClasses cs m = m { classes = cs }

instance HasAttachedProp (Message ms) where
    type AttachedProp (Message ms) = Maybe Txt
    getAttached = attached
    setAttached a m = m { attached = a }

instance HasColorProp (Message ms) where
    getColor = color
    setColor c m = m { color = c }

instance HasCompactProp (Message ms) where
    getCompact = compact
    setCompact c m = m { compact = c }

instance HasErrorProp (Message ms) where
    getError = error
    setError e m = m { error = e }

instance HasFloatingProp (Message ms) where
    getFloating = floating
    setFloating f m = m { floating = f }

instance HasHiddenProp (Message ms) where
    getHidden = hidden
    setHidden h m = m { hidden = h }

instance HasInfoProp (Message ms) where
    getInfo = info
    setInfo i m = m { info = i }

instance HasNegativeProp (Message ms) where
    getNegative = negative
    setNegative n m = m { negative = n }

instance HasOnDismissProp (Message ms) where
    type OnDismissProp (Message ms) = Ef ms IO ()
    getOnDismiss = onDismiss
    setOnDismiss od m = m { onDismiss = od }

instance HasPositiveProp (Message ms) where
    getPositive = positive
    setPositive p m = m { positive = p }

instance HasSizeProp (Message ms) where
    getSize = size
    setSize s m = m { size = s }

instance HasSuccessProp (Message ms) where
    getSuccess = success
    setSuccess s m = m { success = s }

instance HasVisibleProp (Message ms) where
    getVisible = visible
    setVisible v m = m { visible = v }

instance HasWarningProp (Message ms) where
    getWarning = warning
    setWarning w m = m { warning = w }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content mc = View mc

instance Pure Content ms where
    render Content_ {..} =
        let
            cs =
                ( "content"
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
    setAs a mc = mc { as = a }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes as mc = mc { attributes = as }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs mc = mc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs mc = mc { classes = cs }

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
    } deriving (Generic)

instance Default (Item ms) where
    def = (G.to gdef) { as = Li }

pattern Item :: Item ms -> View ms
pattern Item mi = View mi

instance Pure Item ms where
    render Item_ {..} =
        let
            cs =
                ( "content"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
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

data List ms = List_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (List ms) where
    def = (G.to gdef) { as = Ul }

pattern List :: List ms -> View ms
pattern List ml = View ml

instance Pure List ms where
    render List_ {..} =
        let
            cs =
                ( "list"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (List ms) where
    type AsProp (List ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a ml = ml { as = a }

instance HasAttributesProp (List ms) where
    type Attribute (List ms) = Feature ms
    getAttributes = attributes
    setAttributes as ml = ml { attributes = as }

instance HasChildrenProp (List ms) where
    type Child (List ms) = View ms
    getChildren = children
    setChildren cs ml = ml { children = cs }

instance HasClassesProp (List ms) where
    getClasses = classes
    setClasses cs ml = ml { classes = cs }

