{-# LANGUAGE UndecidableInstances #-}
module Semantic.Embed
  ( module Properties
  , module Tools
  , Embed(..), pattern Semantic.Embed.Embed
  ) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Monoid
import GHC.Generics as G
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Txt
import Pure.Data.HTML as HTML
import Pure.Data.HTML.Properties (pattern Src)
import Pure.Data.Events
import Pure.Data.URI

import Semantic.Utils hiding (id)

import Semantic.Icon

import Semantic.Properties as Tools ( HasProp(..) )

import Semantic.Properties as Properties
  ( pattern Name, Name(..)
  , pattern As, As(..)
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

data Embed = Embed_
    { as :: Features -> [View] -> View
    , features :: Features
    , children :: [View]
    , active :: Bool
    , aspectRatio :: Txt
    , autoplay :: Bool
    , branded :: Bool
    , color :: Txt
    , defaultActive :: Bool
    , hd :: Bool
    , icon :: Icon
    , id :: Txt
    , iframe :: Features
    , placeholder :: Txt
    , source :: Maybe EmbedSource
    , url :: Txt
    } deriving (Generic)

instance Default Embed where
    def = (G.to gdef)
        { as = \fs cs -> Div & Features fs & Children cs
        , icon = def & Name "video play"
        , hd = True
        , color = "#444444"
        }

pattern Embed :: Embed -> Embed
pattern Embed e = e

instance Pure Embed where
    view =
        LibraryComponentIO $ \self ->
            let
                handleClick _ = do
                    Embed_ {..} <- getProps self
                    isActive <- getState self
                    unless active (void $ setState self $ \_ st -> return (not st,return ()))

            in def
                { construct = do
                    Embed_ {..} <- getProps self
                    return defaultActive

                , render = \Embed_ {..} isActive ->
                    let
                        cs =
                            [ "ui"
                            , aspectRatio
                            , isActive # "active"
                            , "embed"
                            ]

                        viewSource YouTube          = "YouTube"
                        viewSource Vimeo            = "Vimeo"
                        viewSource (OtherSource os) = os

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
                        as (features & Classes cs & Pure.Data.Events.OnClick handleClick)
                            [ View $ Icon icon
                            , (placeholder /= mempty) # (HTML.Img <| Class "placeholder" . Src placeholder)
                            , active #
                                (Div <| Class "embed" |>
                                    ((not $ Prelude.null children)
                                      ? children
                                      $ [ Iframe <| Attribute "allowfullscreen" "false"
                                                  . Attribute "frameborder" "0"
                                                  . Attribute "height" "100%"
                                                  . Attribute "scrolling" "no"
                                                  . Attribute "src" src
                                                  . (maybe Prelude.id (\s f -> Attribute "title" ("Embedded content from " <> viewSource s <> ".") f) source)
                                                  . Attribute "width" "100%"
                                                  . Features iframe
                                        ]

                                    )
                                )
                            ]

                }

instance HasProp As Embed where
    type Prop As Embed = Features -> [View] -> View
    getProp _ = as
    setProp _ a sp = sp { as = a }

instance HasFeatures Embed where
    getFeatures = features
    setFeatures as sp = sp { features = as }

instance HasChildren Embed where
    getChildren = children
    setChildren cs sp = sp { children = cs }

instance HasProp Active Embed where
    type Prop Active Embed = Bool
    getProp _ = active
    setProp _ a e = e { active = a }

instance HasProp AspectRatio Embed where
    type Prop AspectRatio Embed = Txt
    getProp _ = aspectRatio
    setProp _ ar e = e { aspectRatio = ar }

instance HasProp Autoplay Embed where
    type Prop Autoplay Embed = Bool
    getProp _ = autoplay
    setProp _ a e = e { autoplay = a }

instance HasProp Branded Embed where
    type Prop Branded Embed = Bool
    getProp _ = branded
    setProp _ b e = e { branded = b }

instance HasProp Color Embed where
    type Prop Color Embed = Txt
    getProp _ = color
    setProp _ c e = e { color = c }

instance HasProp DefaultActive Embed where
    type Prop DefaultActive Embed = Bool
    getProp _ = defaultActive
    setProp _ da e = e { defaultActive = da }

pattern HD :: Embed -> Embed
pattern HD e <- (hd &&& Prelude.id -> (True,e)) where
    HD e = e { hd = True }

pattern EmbedIcon :: Icon -> Embed -> Embed
pattern EmbedIcon i e <- (icon &&& Prelude.id -> (i,e)) where
    EmbedIcon i e = e { icon = i }

pattern EmbedId :: Txt -> Embed -> Embed
pattern EmbedId i e <- (id &&& Prelude.id -> (i,e)) where
    EmbedId i e = e { id = i }

pattern EmbedIframe :: Features -> Embed -> Embed
pattern EmbedIframe fs e <- (iframe &&& Prelude.id -> (fs,e)) where
    EmbedIframe fs e = e { iframe = fs }

instance HasProp Placeholder Embed where
    type Prop Placeholder Embed = Txt
    getProp _ = placeholder
    setProp _ p e = e { placeholder = p }

pattern EmbedSource :: EmbedSource -> Embed -> Embed
pattern EmbedSource es e <- (source &&& Prelude.id -> (Just es,e)) where
    EmbedSource es e = e { source = Just es }

instance HasProp URL Embed where
    type Prop URL Embed = Txt
    getProp _ = url
    setProp _ u e = e { url = u }
