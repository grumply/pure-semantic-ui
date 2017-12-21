module Semantic.Elements.Label (module Semantic.Elements.Label, module Export) where

import GHC.Generics as G
import Pure.View as View

import Semantic.Utils

import Semantic.Elements.Label.LabelDetail as Export
import Semantic.Elements.Label.LabelGroup as Export

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
    , image :: Maybe (View ms)
    , handleClick :: Ef ms IO ()
    , pointing :: Maybe Txt
    , ribbon :: Maybe Txt
    , size :: Txt
    , tag :: Bool
    } deriving (Generic)

instance Default (Label ms) where
    def = (G.to gdef) { as = Div }

pattern Label :: Typeable ms => Label ms -> View ms
pattern Label l = View l

instance Typeable ms => Pure Label ms where
    render Label_ {..} =
        let
            pointingClass =
                -- note the careful class ordering
                ($ "pointing") $ flip (maybe (const nil)) pointing $ \case
                    (oneEq "left" "right"  -> Just lr) -> (lr <<>>)
                    (oneEq "above" "below" -> Just ab) -> (<<>> ab)
                    "" -> id
                    _  -> const nil
            
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
                : image #!? "image"
                : tag # "tag"
                : corner # "corner"
                : ribbon # "ribbon"
                : attached # "attached"
                : "label"
                : classes
                )
        in
            as 
                ( ClassList cs
                : handleClick # onClick handleClick
                : attributes
                )
                children
