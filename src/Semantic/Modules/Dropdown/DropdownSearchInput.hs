module Semantic.Modules.Dropdown.DropdownSearchInput where

import GHC.Generics as G
import Pure.View hiding (Type,Value)
import qualified Pure.View as HTML
import Pure.Lifted (JSV,Node(..),(.#))

import Semantic.Utils

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
                : Attribute "aria-autocomplete" "list"
                : Attribute "autoComplete" "off"
                : mergeClasses $ ClassList cs 
                : attributes
                )
                []
