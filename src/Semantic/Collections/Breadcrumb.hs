module Semantic.Collections.Breadcrumb (module Semantic.Collections.Breadcrumb, module Export) where

import GHC.Generics as G
import Pure.View hiding ((!),Ref,name)

import Semantic.Utils

import Semantic.Collections.Breadcrumb.BreadcrumbDivider as Export
import Semantic.Collections.Breadcrumb.BreadcrumbSection as Export

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Size

import qualified Data.List as List
import Data.Function ((&))
import Semantic.Properties.Active
import Semantic.Properties.Ref

data Breadcrumb ms = Breadcrumb_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , size :: Txt
    } deriving (Generic)

instance Default (Breadcrumb ms) where
    def = (G.to gdef) { as = Div }

pattern Breadcrumb :: Typeable ms => Breadcrumb ms -> View ms
pattern Breadcrumb bc = View bc

instance Typeable ms => Pure Breadcrumb ms where
    render Breadcrumb_ {..} =
        let
            cs =
                ( "ui"
                : size
                : "breadcrumb"
                : classes
                )
        in
            as 
                ( mergeClasses $ ClassList cs
                : attributes
                )
                children

instance HasAsProp (Breadcrumb ms) where
    type AsProp (Breadcrumb ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a bc = bc { as = a }

instance HasAttributesProp (Breadcrumb ms) where
    type Attribute (Breadcrumb ms) = Feature ms
    getAttributes = attributes
    setAttributes as bc = bc { attributes = as }

instance HasChildrenProp (Breadcrumb ms) where
    type Child (Breadcrumb ms) = View ms
    getChildren = children
    setChildren cs bc = bc { children = cs }

instance HasClassesProp (Breadcrumb ms) where
    getClasses = classes
    setClasses cs bc = bc { classes = cs }

instance HasSizeProp (Breadcrumb ms) where
    getSize = size
    setSize sz bc = bc { size = sz }

-- | Shorthand for producing a breadcrumb trail from a list of crumbs and links.
--
-- > breadcrumbs nil [("Home","/store"),("Sweets","/store/sweets")]
-- 
-- Produces: 
--
--   (def :: Breadcrumb ms) & Children 
--       [ BreadcrumbSection $ def 
--            |> Lref "/store" 
--            |> Children [ "Store" ]
--       , BreadcrumbDivider def
--       , BreadcrumbSection $ def 
--            |> Lref "/sweets" 
--            |> Children [ "Sweets" ]
--       ]
--
breadcrumbs :: Typeable ms => Bool -> [View ms] -> [([View ms],Txt)] -> Breadcrumb ms
breadcrumbs activateLast divider = (def !) 
    . List.intersperse (BreadcrumbDivider $ def ! divider) 
    . crumbs
  where
      crumb :: Typeable ms => ([View ms],Txt) -> BreadcrumbSection ms
      crumb (bc,l)  = def ! bc & (l ? Ref (Lref l) $ id)

      crumbs []     = nil
      crumbs [x]    = [ BreadcrumbSection $ Active activateLast (crumb x) ]
      crumbs (x:xs) = BreadcrumbSection (crumb x) : crumbs xs

