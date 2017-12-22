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

import Pure.App hiding (name,(!),(%))
import qualified Pure.App
import qualified Pure.View as HTML

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
      let i = Image def % [ Src "http://via.placeholder.com/200x200" ] & Medium & Circular
      in
        Container def !
            [ LabelGroup def ! 
                  [ Label def ! "Test 1"
                  , Label def ! "Test 2" 
                  ] & Circular
            , ImageGroup def ! [i,i]
            , Div [] [ Button def ! "Test" ]
            , Button def { as = Div } & LabelPosition "right" ! 
                  [ Button def ! [ NamedIcon def "world" ]
                  , Label def & Pointing "left" & Basic ! "World" 
                  ] 
            ]
