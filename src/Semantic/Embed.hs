{-# LANGUAGE UndecidableInstances #-}
module Semantic.Embed
  ( module Properties
  , module Tools
  , Embed(..), pattern Embed
  ) where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (active,onClick,url,color,Name,Width,Embed)
import Pure.Route (encodeURI)

import Semantic.Utils hiding (id)

import Semantic.Icon

import Semantic.Properties as Tools ( HasProp(..), (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( pattern Name, Name(..)
  , pattern As, As(..)
  , pattern Attributes, Attributes(..)
  , pattern Children, Children(..)
  , pattern Classes, Classes(..)
  , pattern Active, Active(..)
  , pattern AspectRatio, AspectRatio(..)
  , pattern Autoplay, Autoplay(..)
  , pattern Branded, Branded(..)
  , pattern Color, Color(..)
  , pattern DefaultActive, DefaultActive(..)
  , pattern OnClick, OnClick(..)
  , pattern Placeholder, Placeholder(..)
  , pattern URL, URL(..)
  )

import Prelude hiding (id)
import qualified Prelude

import Data.Function as Tools ((&))
import Pure.Data.Default as Tools

data EmbedSource = YouTube | Vimeo | OtherSource Txt

data Embed ms = Embed_
    { as :: [Feature ms] -> [View ms] -> View ms
    , attributes :: [Feature ms]
    , children :: [View ms]
    , classes :: [Txt]
    , active :: Bool
    , aspectRatio :: Txt
    , autoplay :: Bool
    , branded :: Bool
    , color :: Txt
    , defaultActive :: Bool
    , hd :: Bool
    , icon :: Icon ms
    , id :: Txt
    , iframe :: [Feature ms]
    , onClick :: Ef ms IO ()
    , placeholder :: Txt
    , source :: Maybe EmbedSource
    , url :: Txt
    } deriving (Generic)

instance Default (Embed ms) where
    def = (G.to gdef)
        { as = Div
        , icon = def & Name "video play"
        , hd = True
        , color = "#444444"
        }

pattern Embed :: VC ms => Embed ms -> View ms
pattern Embed e = View e

instance VC ms => Pure Embed ms where
    render e =
        Component "Semantic.Modules.Embed" e $ \self ->
            let
                handleClick = do
                    Embed_ {..} <- getProps self
                    isActive <- getState self
                    onClick
                    unless active (void $ setState self $ \_ -> not)

            in def
                { construct = do
                    Embed_ {..} <- getProps self
                    return defaultActive

                , renderer = \Embed_ {..} isActive ->
                    let
                        cs =
                            ( "ui"
                            : aspectRatio
                            : isActive # "active"
                            : "embed"
                            : classes
                            )

                        renderSource YouTube          = "YouTube"
                        renderSource Vimeo            = "Vimeo"
                        renderSource (OtherSource os) = os

                        defaultIframeAttributes =
                            [ Attribute "allowfullscreen" "false"
                            , Attribute "frameborder" "0"
                            , Attribute "height" "100%"
                            , Attribute "scrolling" "no"
                            , Attribute "src" src
                            , may (\s -> Attribute "title" ("Embedded content from " <> renderSource s <> ".")) source
                            , Attribute "width" "100%"
                            ]

                        src =
                            case source of
                                Just YouTube ->
                                    mconcat
                                        [ "//www.youtube.com/embed/" <> id
                                        , "?autohide=true"
                                        , "&amp;autoplay=" <> (autoplay ? "true" $ "false")
                                        , "&amp;color=" <> (encodeURI color)
                                        , "&amp;hd=" <> (hd ? "true" $ "false")
                                        , "&amp;jsapi=false"
                                        , "&amp;modestbranding=" <> (branded ? "true" $ "false")
                                        , "&amp;rel=" <> (branded ? "0" $ "1")
                                        ]

                                Just Vimeo ->
                                    mconcat
                                        [ "//player.vimeo.com/video/" <> id
                                        , "?api=false"
                                        , "&amp;autoplay=" <> (autoplay ? "true" $ "false")
                                        , "&amp;byline=false"
                                        , "&amp;color=" <> (encodeURI color)
                                        , "&amp;portrait=false"
                                        , "&amp;title=false"
                                        ]

                                _ -> url


                    in
                        as
                            ( mergeClasses $ ClassList cs
                            : On "click" def (\_ -> return $ Just handleClick)
                            : attributes
                            )
                            [ Icon icon
                            , placeholder # Img [ ClassList [ "placeholder" ], Src placeholder ]  [ ]
                            , active #
                                Div [ ClassList [ "embed" ] ]
                                    $ children
                                        ? children
                                        $ [ Iframe (defaultIframeAttributes ++ iframe) [] ]
                            ]

                }


instance HasProp As (Embed ms) where
    type Prop As (Embed ms) = [Feature ms] -> [View ms] -> View ms
    getProp _ = as
    setProp _ a sp = sp { as = a }

instance HasProp Attributes (Embed ms) where
    type Prop Attributes (Embed ms) = [Feature ms]
    getProp _ = attributes
    setProp _ as sp = sp { attributes = as }

instance HasProp Children (Embed ms) where
    type Prop Children (Embed ms) = [View ms]
    getProp _ = children
    setProp _ cs sp = sp { children = cs }

instance HasProp Classes (Embed ms) where
    type Prop Classes (Embed ms) = [Txt]
    getProp _ = classes
    setProp _ cs sp = sp { classes = cs }

instance HasProp Active (Embed ms) where
    type Prop Active (Embed ms) = Bool
    getProp _ = active
    setProp _ a e = e { active = a }

instance HasProp AspectRatio (Embed ms) where
    type Prop AspectRatio (Embed ms) = Txt
    getProp _ = aspectRatio
    setProp _ ar e = e { aspectRatio = ar }

instance HasProp Autoplay (Embed ms) where
    type Prop Autoplay (Embed ms) = Bool
    getProp _ = autoplay
    setProp _ a e = e { autoplay = a }

instance HasProp Branded (Embed ms) where
    type Prop Branded (Embed ms) = Bool
    getProp _ = branded
    setProp _ b e = e { branded = b }

instance HasProp Color (Embed ms) where
    type Prop Color (Embed ms) = Txt
    getProp _ = color
    setProp _ c e = e { color = c }

instance HasProp DefaultActive (Embed ms) where
    type Prop DefaultActive (Embed ms) = Bool
    getProp _ = defaultActive
    setProp _ da e = e { defaultActive = da }

pattern HD :: Embed ms -> Embed ms
pattern HD e <- (hd &&& Prelude.id -> (True,e)) where
    HD e = e { hd = True }

pattern EmbedIcon :: Icon ms -> Embed ms -> Embed ms
pattern EmbedIcon i e <- (icon &&& Prelude.id -> (i,e)) where
    EmbedIcon i e = e { icon = i }

pattern EmbedId :: Txt -> Embed ms -> Embed ms
pattern EmbedId i e <- (id &&& Prelude.id -> (i,e)) where
    EmbedId i e = e { id = i }

pattern EmbedIframe :: [Feature ms] -> Embed ms -> Embed ms
pattern EmbedIframe fs e <- (iframe &&& Prelude.id -> (fs,e)) where
    EmbedIframe fs e = e { iframe = fs }

instance HasProp OnClick (Embed ms) where
    type Prop OnClick (Embed ms) = Ef ms IO ()
    getProp _ = onClick
    setProp _ oc e = e { onClick = oc }

instance HasProp Placeholder (Embed ms) where
    type Prop Placeholder (Embed ms) = Txt
    getProp _ = placeholder
    setProp _ p e = e { placeholder = p }

pattern EmbedSource :: EmbedSource -> Embed ms -> Embed ms
pattern EmbedSource es e <- (source &&& Prelude.id -> (Just es,e)) where
    EmbedSource es e = e { source = Just es }

instance HasProp URL (Embed ms) where
    type Prop URL (Embed ms) = Txt
    getProp _ = url
    setProp _ u e = e { url = u }
