module Semantic.Modules.Dropdown.DropdownSearchInput where

import GHC.Generics as G
import Pure.View hiding (Type,Value)
import qualified Pure.View as HTML
import Pure.Lifted (JSV,Node(..),(.#))

import Semantic.Utils

import Semantic.Properties as Properties
  ( HasAttributesProp(..), pattern Attributes
  , HasClassesProp(..), pattern Classes
  , HasInputRefProp(..), pattern InputRef
  , HasOnChangeProp(..), pattern OnChange
  , HasTabIndexProp(..), pattern TabIndex
  , HasTypeProp(..), pattern Type
  , HasValueProp(..), pattern Value
  )

data DropdownSearchInput ms = DropdownSearchInput_ 
    { attributes :: [Feature ms] 
    , classes :: [Txt]
    , inputRef :: JSV -> Ef ms IO ()
    , onChange :: Txt -> Ef ms IO ()
    , tabIndex :: Maybe Int
    , _type :: Txt
    , value :: Txt
    } deriving (Generic)

instance Default (DropdownSearchInput ms) where
    def = (G.to gdef) { _type = "text" }

pattern DropdownSearchInput :: DropdownSearchInput ms -> View ms
pattern DropdownSearchInput dh = View dh

instance Pure DropdownSearchInput ms where
    render DropdownSearchInput_ {..} =
        let
            handleChange = return . fmap onChange . ((.# "target") >=> (.# "value")) . evtObj

            cs = 
                ( "search"
                : classes
                )
        in
            Input
                ( HTML.Value value
                : On "change" def handleChange
                : HostRef (\(Node n) -> return . Just $ inputRef n)
                : may Tabindex tabIndex
                : HTML.Type _type
                : Attribute "autoComplete" "off"
                : mergeClasses (ClassList cs : attributes)
                )
                []

instance HasAttributesProp (DropdownSearchInput ms) where
    type Attribute (DropdownSearchInput ms) = Feature ms
    getAttributes = attributes 
    setAttributes cs dsi = dsi { attributes = cs }

instance HasClassesProp (DropdownSearchInput ms) where
    getClasses = classes
    setClasses cs dsi = dsi { classes = cs }

instance HasInputRefProp (DropdownSearchInput ms) where
    type InputRefProp (DropdownSearchInput ms) = JSV -> Ef ms IO ()
    getInputRef = inputRef
    setInputRef ir dsi = dsi { inputRef = ir }

instance HasOnChangeProp (DropdownSearchInput ms) where
    type OnChangeProp (DropdownSearchInput ms) = Txt -> Ef ms IO ()
    getOnChange = onChange
    setOnChange oc dsi = dsi { onChange = oc }

instance HasTabIndexProp (DropdownSearchInput ms) where
    getTabIndex = tabIndex
    setTabIndex ti dsi = dsi { tabIndex = ti }

instance HasTypeProp (DropdownSearchInput ms) where
    getType = _type
    setType t dsi = dsi { _type = t }

instance HasValueProp (DropdownSearchInput ms) where
    getValue = value
    setValue v dsi = dsi { value = v }
