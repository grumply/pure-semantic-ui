module Semantic.Modules.Search (module Semantic.Modules.Search, module Export) where

import GHC.Generics as G
import Pure.View hiding (onFocus,onBlur)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Aligned
import Semantic.Properties.Category
import Semantic.Properties.Fluid
import Semantic.Properties.Focus
import Semantic.Properties.Loading
import Semantic.Properties.OnBlur
import Semantic.Properties.OnFocus
import Semantic.Properties.OnMouseDown
import Semantic.Properties.Open
import Semantic.Properties.Size

import Semantic.Modules.Search.SearchCategory as Export
import Semantic.Modules.Search.SearchResult as Export
import Semantic.Modules.Search.SearchResults as Export

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
