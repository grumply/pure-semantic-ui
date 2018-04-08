module Semantic.Views.Feed where

import GHC.Generics as G hiding (Meta)
import Pure.View hiding (text,Event)

import Semantic.Utils

import Semantic.Elements.Image (Image(..))

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasSizeProp(..), pattern Size
  , HasIsTextProp(..), pattern IsText
  )

data Feed ms = Feed_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Feed ms) where
    def = (G.to gdef) { as = Div }

pattern Feed :: Feed ms -> View ms
pattern Feed f = View f

instance Pure Feed ms where
    render Feed_ {..} =
        let
            cs =
                ( "ui"
                : size
                : "feed"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Feed ms) where
    type AsProp (Feed ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a f = f { as = a }

instance HasAttributesProp (Feed ms) where
    type Attribute (Feed ms) = Feature ms
    getAttributes = attributes
    setAttributes as f = f { attributes = as }

instance HasChildrenProp (Feed ms) where
    type Child (Feed ms) = View ms
    getChildren = children
    setChildren cs f = f { children = cs }

instance HasClassesProp (Feed ms) where
    getClasses = classes
    setClasses cs f = f { classes = cs }

instance HasSizeProp (Feed ms) where
    getSize = size
    setSize s f = f { size = s }

data Content ms = Content_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Content ms) where
    def = (G.to gdef) { as = Div }

pattern Content :: Content ms -> View ms
pattern Content fc = View fc

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
    setAs a fc = fc { as = a }

instance HasAttributesProp (Content ms) where
    type Attribute (Content ms) = Feature ms
    getAttributes = attributes
    setAttributes as fc = fc { attributes = as }

instance HasChildrenProp (Content ms) where
    type Child (Content ms) = View ms
    getChildren = children
    setChildren cs fc = fc { children = cs }

instance HasClassesProp (Content ms) where
    getClasses = classes
    setClasses cs fc = fc { classes = cs }

data Date ms = Date_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Date ms) where
    def = (G.to gdef) { as = Div }

pattern Date :: Date ms -> View ms
pattern Date fd = View fd

instance Pure Date ms where
    render Date_ {..} =
        let
            cs =
                ( "date"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Date ms) where
    type AsProp (Date ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fd = fd { as = a }

instance HasAttributesProp (Date ms) where
    type Attribute (Date ms) = Feature ms
    getAttributes = attributes
    setAttributes as fd = fd { attributes = as }

instance HasChildrenProp (Date ms) where
    type Child (Date ms) = View ms
    getChildren = children
    setChildren cs fd = fd { children = cs }

instance HasClassesProp (Date ms) where
    getClasses = classes
    setClasses cs fd = fd { classes = cs }

data Event ms = Event_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Event ms) where
    def = (G.to gdef) { as = Div }

pattern Event :: Event ms -> View ms
pattern Event fe = View fe

instance Pure Event ms where
    render Event_ {..} =
        let
            cs =
                ( "event"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Event ms) where
    type AsProp (Event ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fe = fe { as = a }

instance HasAttributesProp (Event ms) where
    type Attribute (Event ms) = Feature ms
    getAttributes = attributes
    setAttributes as fe = fe { attributes = as }

instance HasChildrenProp (Event ms) where
    type Child (Event ms) = View ms
    getChildren = children
    setChildren cs fe = fe { children = cs }

instance HasClassesProp (Event ms) where
    getClasses = classes
    setClasses cs fe = fe { classes = cs }

data Extra ms = Extra_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , text :: Bool
    } deriving (Generic)

instance Default (Extra ms) where
    def = (G.to gdef) { as = Div }

pattern Extra :: Extra ms -> View ms
pattern Extra fe = View fe

instance Pure Extra ms where
    render Extra_ {..} =
        let
            image = foldPures (\(Image_ {}) -> const True) False children

            cs =
                ( image # "images"
                : text # "text"
                : "extra"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Extra ms) where
    type AsProp (Extra ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fe = fe { as = a }

instance HasAttributesProp (Extra ms) where
    type Attribute (Extra ms) = Feature ms
    getAttributes = attributes
    setAttributes as fe = fe { attributes = as }

instance HasChildrenProp (Extra ms) where
    type Child (Extra ms) = View ms
    getChildren = children
    setChildren cs fe = fe { children = cs }

instance HasClassesProp (Extra ms) where
    getClasses = classes
    setClasses cs fe = fe { classes = cs }

instance HasIsTextProp (Extra ms) where
    getIsText = text
    setIsText it fe = fe { text = it }

data Label ms = Label_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Label ms) where
    def = (G.to gdef) { as = Div }

pattern Label :: Label ms -> View ms
pattern Label fl = View fl

instance Pure Label ms where
    render Label_ {..} =
        let
            cs =
                ( "label"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Label ms) where
    type AsProp (Label ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fl = fl { as = a }

instance HasAttributesProp (Label ms) where
    type Attribute (Label ms) = Feature ms
    getAttributes = attributes
    setAttributes as fl = fl { attributes = as }

instance HasChildrenProp (Label ms) where
    type Child (Label ms) = View ms
    getChildren = children
    setChildren cs fl = fl { children = cs }

instance HasClassesProp (Label ms) where
    getClasses = classes
    setClasses cs fl = fl { classes = cs }

data Like ms = Like_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Like ms) where
    def = (G.to gdef) { as = Div }

pattern Like :: Like ms -> View ms
pattern Like fl = View fl

instance Pure Like ms where
    render Like_ {..} =
        let
            cs =
                ( "like"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Like ms) where
    type AsProp (Like ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fl = fl { as = a }

instance HasAttributesProp (Like ms) where
    type Attribute (Like ms) = Feature ms
    getAttributes = attributes
    setAttributes as fl = fl { attributes = as }

instance HasChildrenProp (Like ms) where
    type Child (Like ms) = View ms
    getChildren = children
    setChildren cs fl = fl { children = cs }

instance HasClassesProp (Like ms) where
    getClasses = classes
    setClasses cs fl = fl { classes = cs }

data Meta ms = Meta_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Meta ms) where
    def = (G.to gdef) { as = Div }

pattern Meta :: Meta ms -> View ms
pattern Meta fm = View fm

instance Pure Meta ms where
    render Meta_ {..} =
        let
            cs =
                ( "meta"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Meta ms) where
    type AsProp (Meta ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fm = fm { as = a }

instance HasAttributesProp (Meta ms) where
    type Attribute (Meta ms) = Feature ms
    getAttributes = attributes
    setAttributes as fm = fm { attributes = as }

instance HasChildrenProp (Meta ms) where
    type Child (Meta ms) = View ms
    getChildren = children
    setChildren cs fm = fm { children = cs }

instance HasClassesProp (Meta ms) where
    getClasses = classes
    setClasses cs fm = fm { classes = cs }

data Summary ms = Summary_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (Summary ms) where
    def = (G.to gdef) { as = Div }

pattern Summary :: Summary ms -> View ms
pattern Summary fs = View fs

instance Pure Summary ms where
    render Summary_ {..} =
        let
            cs =
                ( "summary"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Summary ms) where
    type AsProp (Summary ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fs = fs { as = a }

instance HasAttributesProp (Summary ms) where
    type Attribute (Summary ms) = Feature ms
    getAttributes = attributes
    setAttributes as fs = fs { attributes = as }

instance HasChildrenProp (Summary ms) where
    type Child (Summary ms) = View ms
    getChildren = children
    setChildren cs fs = fs { children = cs }

instance HasClassesProp (Summary ms) where
    getClasses = classes
    setClasses cs fs = fs { classes = cs }

data User ms = User_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    } deriving (Generic)

instance Default (User ms) where
    def = (G.to gdef) { as = A }

pattern User :: User ms -> View ms
pattern User fu = View fu

instance Pure User ms where
    render User_ {..} =
        let
            cs =
                ( "user"
                : classes
                )
        in
            as
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (User ms) where
    type AsProp (User ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a fu = fu { as = a }

instance HasAttributesProp (User ms) where
    type Attribute (User ms) = Feature ms
    getAttributes = attributes
    setAttributes as fu = fu { attributes = as }

instance HasChildrenProp (User ms) where
    type Child (User ms) = View ms
    getChildren = children
    setChildren cs fu = fu { children = cs }

instance HasClassesProp (User ms) where
    getClasses = classes
    setClasses cs fu = fu { classes = cs }
