module Semantic.Properties.Classes where

import Pure.View

import Semantic.Properties.Utils

class HasClassesProp a where
    getClasses :: a -> [Txt]
    setClasses :: [Txt] -> a -> a

pattern Classes :: HasClassesProp a => [Txt] -> a -> a
pattern Classes cs a <- (getClasses &&& id -> (cs,a)) where
    Classes cs a = setClasses cs a
       
-- instance HasClassesProp (View ms) where
--     getClasses v =
--         case v of
--             HTMLView _ _ fs _    -> fromMaybe [] . getLast . foldMap getClasses' $ fs
--             KHTMLView _ _ fs _ _ -> fromMaybe [] . getLast . foldMap getClasses' $ fs
--             SVGView _ _ fs _     -> fromMaybe [] . getLast . foldMap getClasses' $ fs
--             KSVGView _ _ fs _ _  -> fromMaybe [] . getLast . foldMap getClasses' $ fs
--             ManagedView _ _ fs _ -> fromMaybe [] . getLast . foldMap getClasses' $ fs
--             _                    -> []
--       where
--         getClasses' (ClassList cs) = Last (Just cs)
--         getClasses' _ = Last Nothing

--     setClasses cs v =
--         case v of
--             HTMLView e t fs cs    -> HTMLView e t (setClasses' fs) cs
--             KHTMLView e t fs cs m -> KHTMLView e t (setClasses' fs) cs m
--             SVGView e t fs cs     -> SVGView e t (setClasses' fs) cs
--             KSVGView e t fs cs m  -> KSVGView e t (setClasses' fs) cs m
--             ManagedView h t fs c  -> ManagedView h t (setClasses' fs) c
--             _                     -> v
--       where
--         setClasses' (Attribute "class" _ : rest) = (ClassList cs) : setClasses' rest
--         setClasses' (x : rest) = x : setClasses' rest
