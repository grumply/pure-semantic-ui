module Semantic.Modules.Search where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (onFocus,onBlur,name,active,Result,onClick)

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasAlignedProp(..), pattern Aligned
  , HasCategoryProp(..), pattern Category
  , HasFluidProp(..), pattern Fluid
  , HasFocusProp(..), pattern Focus
  , HasLoadingProp(..), pattern Loading
  , HasOnBlurProp(..), pattern OnBlur
  , HasOnFocusProp(..), pattern OnFocus
  , HasOnMouseDownProp(..), pattern OnMouseDown
  , HasOpenProp(..), pattern Open
  , HasSizeProp(..), pattern Size
  , HasActiveProp(..), pattern Active
  , HasNameProp(..), pattern Name
  , HasResultsProp(..), pattern Results
  , HasOnClickProp(..), pattern OnClick
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

instance HasAsProp (Search ms) where
    type AsProp (Search ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f s = s { as = f }

instance HasAttributesProp (Search ms) where
    type Attribute (Search ms) = Feature ms
    getAttributes = attributes
    setAttributes cs s = s { attributes = cs }

instance HasChildrenProp (Search ms) where
    type Child (Search ms) = View ms
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasClassesProp (Search ms) where
    getClasses = classes
    setClasses cs s = s { classes = cs }

instance HasAlignedProp (Search ms) where
    getAligned = aligned
    setAligned a s = s { aligned = a }

instance HasCategoryProp (Search ms) where
    getCategory = category
    setCategory c s = s { category = c }

instance HasFluidProp (Search ms) where
    getFluid = fluid
    setFluid f s = s { fluid = f }

instance HasFocusProp (Search ms) where
    getFocus = focus
    setFocus f s = s { focus = f }

instance HasLoadingProp (Search ms) where
    getLoading = loading
    setLoading l s = s { loading = l }

instance HasOnBlurProp (Search ms) where
    type OnBlurProp (Search ms) = Ef ms IO ()
    getOnBlur = onBlur
    setOnBlur ob s = s { onBlur = ob }

instance HasOnFocusProp (Search ms) where
    type OnFocusProp (Search ms) = Ef ms IO ()
    getOnFocus = onFocus
    setOnFocus onf s = s { onFocus = onf }

instance HasOnMouseDownProp (Search ms) where
    type OnMouseDownProp (Search ms) = Ef ms IO ()
    getOnMouseDown = onMouseDown
    setOnMouseDown omd s = s { onMouseDown = omd }

instance HasOpenProp (Search ms) where
    getOpen = open
    setOpen o s = s { open = o }

instance HasSizeProp (Search ms) where
    getSize = size
    setSize sz s = s { size = sz }

data Category result ms = Category_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , name :: Txt
    , results :: [result]
    , renderCategory :: Category result ms -> [View ms]
    } deriving (Generic)

instance Default (Category result ms) where
    def = (G.to gdef)
        { as = Div
        , renderCategory = fromTxt . name
        }

pattern Category :: Typeable result => Category result ms -> View ms
pattern Category sc = View sc

instance Typeable result => Pure (Category result) ms where
    render sc@Category_ {..} =
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
                ( Div [ ClassList [ "name" ] ] (renderCategory sc)
                : children
                )

instance HasAsProp (Category result ms) where
    type AsProp (Category result ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sc = sc { as = f }

instance HasAttributesProp (Category result ms) where
    type Attribute (Category result ms) = Feature ms
    getAttributes = attributes
    setAttributes cs sc = sc { attributes = cs }

instance HasChildrenProp (Category result ms) where
    type Child (Category result ms) = View ms
    getChildren = children
    setChildren cs sc = sc { children = cs }

instance HasClassesProp (Category result ms) where
    getClasses = classes
    setClasses cs sc = sc { classes = cs }

instance HasActiveProp (Category result ms) where
    getActive = active
    setActive a sc = sc { active = a }

instance HasNameProp (Category result ms) where
    getName = name
    setName n sc = sc { name = n }

instance HasResultsProp (Category result ms) where
    type ResultsProp (Category result ms) = [result]
    getResults = results
    setResults rs sc = sc { results = rs }

pattern RenderCategory :: (Category result ms -> [View ms]) -> Category result ms -> Category result ms
pattern RenderCategory rsc sc <- (renderCategory &&& id -> (rsc,sc)) where
    RenderCategory rsc sc = sc { renderCategory = rsc }

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

instance HasAsProp (Result ms) where
    type AsProp (Result ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sr = sr { as = f }

instance HasAttributesProp (Result ms) where
    type Attribute (Result ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs sr = sr { attributes = cs }

instance HasChildrenProp (Result ms) where
    type Child (Result ms) = View ms
    getChildren = children
    setChildren cs sr = sr { children = cs }

instance HasClassesProp (Result ms) where
    getClasses = classes
    setClasses cs sr = sr { classes = cs }

instance HasActiveProp (Result ms) where
    getActive = active
    setActive a sr = sr { active = a }

instance HasOnClickProp (Result ms) where
    type OnClickProp (Result ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc sr = sr { onClick = oc }

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

instance HasAsProp (Results ms) where
    type AsProp (Results ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs f sr = sr { as = f }

instance HasAttributesProp (Results ms) where
    type Attribute (Results ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs sr = sr { attributes = cs }

instance HasChildrenProp (Results ms) where
    type Child (Results ms) = View ms
    getChildren = children
    setChildren cs sr = sr { children = cs }

instance HasClassesProp (Results ms) where
    getClasses = classes
    setClasses cs sr = sr { classes = cs }
