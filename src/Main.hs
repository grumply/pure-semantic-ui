{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Pure.App
import qualified Pure.App
import Pure.View as HTML

import Semantic

data Routes = HomeR deriving Eq

main = run App {..}
  where
    key = "my-app"
    build = return
    prime = return ()
    root = Nothing
    routes = Pure.App.dispatch HomeR
    pages HomeR = pure $ page _Head _Home

_Head = simple "Head" $
  Head [] [ HTML.Link [ Rel "stylesheet", Href "http://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.2.12/semantic.min.css" ] [] ] 

data HomeState ms = HomeState

_Home = Controller {..}
  where
    key = "Home"
    build = return
    prime = return ()
    model = HomeState
    view HomeState = 
      Container def 
        { children = 
            [ LabelGroup def 
                { children = 
                    [ Semantic.Label def { children = [ "Test 1" ] } 
                    , Semantic.Label def { children = [ "Test 2" ] }
                    ] 
                }
            ]
        } & TextAlignLeft 
