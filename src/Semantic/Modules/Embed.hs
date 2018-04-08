{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Embed
  ( module Properties
  , module Tools
  , Embed(..), pattern Embed
  ) where

import Control.Arrow ((&&&))
import GHC.Generics as G
import Pure.View hiding (active,onClick,url,color,Name,Width,Embed)
import Pure.Route (encodeURI)

import Semantic.Utils hiding (id)

import Semantic.Elements.Icon

import Semantic.Properties as Tools ( (<|), (<||>), (|>) )

import Semantic.Properties as Properties
  ( HasNameProp(..), pattern Name
  , HasAsProp(..), pattern As
  , HasAttributesProp(..), pattern Attributes
  , HasChildrenProp(..), pattern Children
  , HasClassesProp(..), pattern Classes
  , HasActiveProp(..), pattern Active
  , HasAspectRatioProp(..), pattern AspectRatio
  , HasAutoplayProp(..), pattern Autoplay
  , HasBrandedProp(..), pattern Branded
  , HasColorProp(..), pattern Color
  , HasDefaultActiveProp(..), pattern DefaultActive
  , HasOnClickProp(..), pattern OnClick
  , HasPlaceholderProp(..), pattern Placeholder
  , HasURLProp(..), pattern URL
  )

import Prelude hiding (id)
import qualified Prelude

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


instance HasAsProp (Embed ms) where
    type AsProp (Embed ms) = [Feature ms] -> [View ms] -> View ms
    getAs = as
    setAs a sp = sp { as = a }

instance HasAttributesProp (Embed ms) where
    type Attribute (Embed ms) = Feature ms
    getAttributes = attributes
    setAttributes as sp = sp { attributes = as }

instance HasChildrenProp (Embed ms) where
    type Child (Embed ms) = View ms
    getChildren = children
    setChildren cs sp = sp { children = cs }

instance HasClassesProp (Embed ms) where
    getClasses = classes
    setClasses cs sp = sp { classes = cs }

instance HasActiveProp (Embed ms) where
    getActive = active
    setActive a e = e { active = a }

instance HasAspectRatioProp (Embed ms) where
    getAspectRatio = aspectRatio
    setAspectRatio ar e = e { aspectRatio = ar }

instance HasAutoplayProp (Embed ms) where
    getAutoplay = autoplay
    setAutoplay a e = e { autoplay = a }

instance HasBrandedProp (Embed ms) where
    getBranded = branded
    setBranded b e = e { branded = b }

instance HasColorProp (Embed ms) where
    getColor = color
    setColor c e = e { color = c }

instance HasDefaultActiveProp (Embed ms) where
    getDefaultActive = defaultActive
    setDefaultActive da e = e { defaultActive = da }

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

instance HasOnClickProp (Embed ms) where
    type OnClickProp (Embed ms) = Ef ms IO ()
    getOnClick = onClick
    setOnClick oc e = e { onClick = oc }

instance HasPlaceholderProp (Embed ms) where
    getPlaceholder = placeholder
    setPlaceholder p e = e { placeholder = p }

pattern EmbedSource :: EmbedSource -> Embed ms -> Embed ms
pattern EmbedSource es e <- (source &&& Prelude.id -> (Just es,e)) where
    EmbedSource es e = e { source = Just es }

instance HasURLProp (Embed ms) where
    getURL = url
    setURL u e = e { url = u }
