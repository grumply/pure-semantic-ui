module Semantic.Collections.Message (module Semantic.Collections.Message, module Export) where

import GHC.Generics as G
import Pure.View hiding (color,hidden,visible,Name)

import Semantic.Utils

import Semantic.Elements.Icon
import Semantic.Properties.Name

import Semantic.Properties.As
import Semantic.Properties.Attributes
import Semantic.Properties.Children
import Semantic.Properties.Classes
import Semantic.Properties.Attached
import Semantic.Properties.Color
import Semantic.Properties.Compact
import Semantic.Properties.Error
import Semantic.Properties.Floating
import Semantic.Properties.Hidden
import Semantic.Properties.Info
import Semantic.Properties.Negative
import Semantic.Properties.OnDismiss
import Semantic.Properties.Positive
import Semantic.Properties.Size
import Semantic.Properties.Success
import Semantic.Properties.Visible
import Semantic.Properties.Warning

import Prelude hiding (error)

import Semantic.Collections.Message.MessageContent as Export
import Semantic.Collections.Message.MessageHeader as Export
import Semantic.Collections.Message.MessageItem as Export

data Message ms = Message_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , attached :: Maybe Txt
    , color :: Txt
    , compact :: Bool
    , error :: Bool
    , floating :: Bool
    , hidden :: Bool
    , info :: Bool
    , negative :: Bool
    , onDismiss :: Ef ms IO ()
    , positive :: Bool
    , size :: Txt
    , success :: Bool
    , visible :: Bool
    , warning :: Bool
    } deriving (Generic)

instance Default (Message ms) where
    def = (G.to gdef) { as = Div }

pattern Message :: Typeable ms => Message ms -> View ms
pattern Message m = View m

instance Typeable ms => Pure Message ms where
    render Message_ {..} =
        let
            icon = foldPures (\(Icon_ {}) -> const True) False children

            dismissIcon = onDismiss # (Icon $ def & Name "close" & Attributes [ On "click" def (\_ -> return $ Just onDismiss) ])

            cs =
                ( "ui"
                : color
                : size
                : compact # "compact"
                : error # "error"
                : floating # "floating"
                : hidden # "hidden"
                : icon # "icon"
                : info # "info"
                : negative # "negative"
                : positive # "positive"
                : success # "success"
                : visible # "visible"
                : warning # "warning"
                : may (<>> "attached") attached 
                : "message"
                : classes
                )
        in
            as
                ( ClassList cs
                : attributes
                )
                ( dismissIcon : children )

instance HasAsProp (Message ms) where
    type AsProp (Message ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a m = m { as = a }

instance HasAttributesProp (Message ms) where
    type Attribute (Message ms) = Feature ms
    getAttributes = attributes
    setAttributes as m = m { attributes = as }

instance HasChildrenProp (Message ms) where
    type Child (Message ms) = View ms
    getChildren = children
    setChildren cs m = m { children = cs }

instance HasClassesProp (Message ms) where
    getClasses = classes
    setClasses cs m = m { classes = cs }

instance HasAttachedProp (Message ms) where
    type AttachedProp (Message ms) = Maybe Txt
    getAttached = attached
    setAttached a m = m { attached = a }

instance HasColorProp (Message ms) where
    getColor = color
    setColor c m = m { color = c }

instance HasCompactProp (Message ms) where
    getCompact = compact
    setCompact c m = m { compact = c }

instance HasErrorProp (Message ms) where
    getError = error
    setError e m = m { error = e }

instance HasFloatingProp (Message ms) where
    getFloating = floating
    setFloating f m = m { floating = f }

instance HasHiddenProp (Message ms) where
    getHidden = hidden
    setHidden h m = m { hidden = h }

instance HasInfoProp (Message ms) where
    getInfo = info
    setInfo i m = m { info = i }

instance HasNegativeProp (Message ms) where
    getNegative = negative
    setNegative n m = m { negative = n }

instance HasOnDismissProp (Message ms) where
    type OnDismissProp (Message ms) = Ef ms IO ()
    getOnDismiss = onDismiss
    setOnDismiss od m = m { onDismiss = od }

instance HasPositiveProp (Message ms) where
    getPositive = positive
    setPositive p m = m { positive = p }

instance HasSizeProp (Message ms) where
    getSize = size
    setSize s m = m { size = s }

instance HasSuccessProp (Message ms) where
    getSuccess = success
    setSuccess s m = m { success = s }

instance HasVisibleProp (Message ms) where
    getVisible = visible
    setVisible v m = m { visible = v }

instance HasWarningProp (Message ms) where
    getWarning = warning
    setWarning w m = m { warning = w }
