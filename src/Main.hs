{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
module Main where

import Pure.App
import qualified Pure.App
import Pure.View as HTML hiding (Label)

import Semantic

import Debug.Trace

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
      Container def & Children
          [ LabelGroup def & Children
                [ Label def & Children [ "Test 1" ]
                , Label def & Children [ "Test 2" ]
                ] 
          , Image def & Attributes [ Src "http://via.placeholder.com/350x150" ]
          , Image def & Attributes [ Src "http://via.placeholder.com/350x150" ]
          , ImageGroup def & Children
                [ Image def & Attributes [ Src "http://via.placeholder.com/350x150" ]
                , Image def & Attributes [ Src "http://via.placeholder.com/350x150" ]
                ]
          ]