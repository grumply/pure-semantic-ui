module Semantic.Modules.Search (module Semantic.Modules.Search, module Export) where

import GHC.Generics as G
import Pure.View hiding (onFocus,onBlur)

import Semantic.Utils

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
               : onFocus # On "focus" def (\_ -> return $ Just onFocus)
               : onBlur # On "blur" def (\_ -> return $ Just onBlur)
               : attributes
               )
               children
