module Semantic.Elements.Step (module Semantic.Elements.Step, module Export) where

import GHC.Generics as G
import Pure.View hiding (active,completed,disabled,onClick)

import Semantic.Utils

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Active
import Semantic.Properties.Completed
import Semantic.Properties.Disabled
import Semantic.Properties.Ref
import Semantic.Properties.Link
import Semantic.Properties.OnClick
import Semantic.Properties.Ordered

import Semantic.Elements.Step.StepContent as Export
import Semantic.Elements.Step.StepDescription as Export
import Semantic.Elements.Step.StepGroup as Export
import Semantic.Elements.Step.StepTitle as Export

data Step ms = Step_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , completed :: Bool
    , disabled :: Bool
    , ref :: Feature ms
    , link :: Bool
    , onClick :: Ef ms IO ()
    , ordered :: Bool
    } deriving (Generic)

instance Default (Step ms) where
    def = (G.to gdef) { as = Div }

pattern Step :: Step ms -> View ms
pattern Step s = View s

instance Pure Step ms where
    render Step_ {..} =
        let
            e = onClick ? A $ as

            cs =
                ( active # "active"
                : completed # "completed"
                : disabled # "disabled"
                : link # "link"
                : "step"
                : classes
                )
        in
            e
                ( mergeClasses $ ClassList cs 
                : ref
                : onClick # (disabled #! On "click" def (\_ -> return $ Just onClick))
                : attributes
                )
                children

instance HasAsProp (Step ms) where
    type AsProp (Step ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a s = s { as = a }

instance HasAttributesProp (Step ms) where
    type Attribute (Step ms) = Feature ms
    getAttributes = attributes
    setAttributes as s = s { attributes = as }

instance HasChildrenProp (Step ms) where
    type Child (Step ms) = View ms
    getChildren = children
    setChildren cs s = s { children = cs }

instance HasClassesProp (Step ms) where
    getClasses = classes
    setClasses cs s = s { classes = cs }

instance HasActiveProp (Step ms) where
    getActive = active
    setActive a s = s { active = a }

instance HasCompletedProp (Step ms) where
    getCompleted = completed
    setCompleted c s = s { completed = c }

instance HasDisabledProp (Step ms) where
    getDisabled = disabled
    setDisabled d s = s { disabled = d }

instance HasRefProp (Step ms) where
    type RefProp (Step ms) = Feature ms
    getRef = ref
    setRef r s = s { ref = r }

instance HasLinkProp (Step ms) where
    getLink = link
    setLink l s = s { link = l }

instance HasOnClickProp (Step ms) where
    type OnClickProp (Step ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc s = s { onClick = oc }

instance HasOrderedProp (Step ms) where
    getOrdered = ordered
    setOrdered o s = s { ordered = o }