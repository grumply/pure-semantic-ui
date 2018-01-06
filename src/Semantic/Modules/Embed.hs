{-# LANGUAGE UndecidableInstances #-}
module Semantic.Modules.Embed where

import GHC.Generics as G
import Pure.View hiding (color,Name,Width)
import Pure.Route (encodeURI)

import Semantic.Utils

import Semantic.Elements.Icon

import Semantic.Properties.Name

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
