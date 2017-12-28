module Semantic.Collections.Form where

import GHC.Generics as G
import Pure.View hiding (name,Form)
import qualified Pure.View as HTML

import Semantic.Utils

data Form ms = Form_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , action :: Txt
    , error :: Bool
    , inverted :: Bool
    , loading :: Bool
    , onSubmit :: Ef ms IO ()
    , reply :: Bool
    , size :: Txt
    , success :: Bool
    , unstackable :: Bool
    , warning :: Bool
    , widths :: Width
    } deriving (Generic)

instance Default (Form ms) where
    def = (G.to gdef) { as = HTML.Form }

pattern Form :: Typeable ms => Form ms -> View ms
pattern Form f = View f

instance Typeable ms => Pure Form ms where
    render Form_ {..} =
        let
            cs = 
                ( "ui"
                : size
                : error # "error"
                : inverted # "inverted"
                : loading # "loading"
                : reply # "reply"
                : success # "success"
                : unstackable # "unstackable"
                : warning # "warning"
                : widthProp widths def True
                : "form"
                : classes
                )
        in
            as
                ( ClassList cs
                : Prop "action" action
                : onSubmit # (On "submit" def { preventDef = True } (\_ -> return $ Just onSubmit))
                : attributes
                )
                children