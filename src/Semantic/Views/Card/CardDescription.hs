import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.TextAlign


instance HasAsProp (CardDescription ms) where
    type AsProp (CardDescription ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a cd = cd { as = a }

instance HasAttributesProp (CardDescription ms) where
    type Attribute (CardDescription ms) = Feature ms
    getAttributes = attributes
    setAttributes as cd = cd { attributes = as }

instance HasChildrenProp (CardDescription ms) where
    type Child (CardDescription ms) = View ms
    getChildren = children
    setChildren cs cd = cd { children = cs }

instance HasClassesProp (CardDescription ms) where
    getClasses = classes
    setClasses cs cd = cd { classes = cs }

instance HasTextAlignProp (CardDescription ms) where
    getTextAlign = textAlign
    setTextAlign ta cc = cc { textAlign = ta }