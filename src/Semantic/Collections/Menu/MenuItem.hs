module Semantic.Collections.Menu.MenuItem where

import GHC.Generics as G
import Pure.View hiding (active,color,disabled,onClick,position)

import Semantic.Utils

import Semantic.Elements.Icon

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Color
import Semantic.Properties.Disabled
import Semantic.Properties.Fitted
import Semantic.Properties.Index
import Semantic.Properties.IsHeader
import Semantic.Properties.Link
import Semantic.Properties.OnClick
import Semantic.Properties.Position

data MenuItem ms = MenuItem_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , color :: Txt
    , disabled :: Bool
    , fitted :: Maybe Txt
    , header :: Bool
    , index :: Int
    , link :: Bool
    , onClick :: Ef ms IO ()
    , position :: Txt
    } deriving (Generic)

instance Default (MenuItem ms) where
    def = (G.to gdef) { as = Div }

pattern MenuItem :: MenuItem ms -> View ms
pattern MenuItem mi = View mi

instance Pure MenuItem ms where
    render MenuItem_ {..} =
        let
            e = onClick ? A $ as

            icon = 
                case children of
                    [ Icon i ] -> True
                    _          -> False

            cs =
                ( color
                : position
                : active # "active"
                : disabled # "disabled"
                : icon # "icon"
                : header # "header"
                : link # "link"
                : may ("fitted" <<>>) fitted
                : "item"
                : classes
                )

        in
            e
                ( mergeClasses $ ClassList cs
                : onClick # (On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasAsProp (MenuItem ms) where
    type AsProp (MenuItem ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a mi = mi { as = a }

instance HasAttributesProp (MenuItem ms) where
    type Attribute (MenuItem ms) = Feature ms
    getAttributes = attributes
    setAttributes as mi = mi { attributes = as }

instance HasChildrenProp (MenuItem ms) where
    type Child (MenuItem ms) = View ms
    getChildren = children
    setChildren cs mi = mi { children = cs }

instance HasClassesProp (MenuItem ms) where
    getClasses = classes
    setClasses cs mi = mi { classes = cs }

instance HasActiveProp (MenuItem ms) where
    getActive = active
    setActive a mi = mi { active = a }

instance HasColorProp (MenuItem ms) where
    getColor = color
    setColor c mi = mi { color = c }

instance HasDisabledProp (MenuItem ms) where
    getDisabled = disabled
    setDisabled d mi = mi { disabled = d }

instance HasFittedProp (MenuItem ms) where
    type FittedProp (MenuItem ms) = Maybe Txt
    getFitted = fitted
    setFitted f mi = mi { fitted = f }

instance HasIsHeaderProp (MenuItem ms) where
    getIsHeader = header
    setIsHeader h mi = mi { header = h }

instance HasIndexProp (MenuItem ms) where
    getIndex = index
    setIndex i mi = mi { index = i }

instance HasLinkProp (MenuItem ms) where
    getLink = link
    setLink l mi = mi { link = l }

instance HasOnClickProp (MenuItem ms) where
    type OnClickProp (MenuItem ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc mi = mi { onClick = oc }

instance HasPositionProp (MenuItem ms) where
    getPosition = position
    setPosition p mi = mi { position = p }
