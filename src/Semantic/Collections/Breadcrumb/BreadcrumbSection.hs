module Semantic.Collections.Breadcrumb.BreadcrumbSection where

import GHC.Generics as G
import Pure.View hiding (name)

import Semantic.Utils

data BreadcrumbSection ms = BreadcrumbSection_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms] 
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    } deriving (Generic)

instance Default (BreadcrumbSection ms) where
    def = (G.to gdef) { as = Div }

pattern BreadcrumbSection :: Typeable ms => BreadcrumbSection ms -> View ms
pattern BreadcrumbSection bcs = View bcs

instance Typeable ms => Pure BreadcrumbSection ms where
    render BreadcrumbSection_ {..} =
        let
            e = link ? A $ ref ? A $ as
            cs = 
                ( active # "active"
                : "section"
                : classes
                )
        in
            e
                ( ClassList cs
                : ref
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children