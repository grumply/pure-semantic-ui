{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Pure.App
import qualified Pure.App
import Pure.View

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
  Head [] []

data HomeState ms = HomeState

_Home = Controller {..}
  where
    key = "Home"
    build = return
    prime = return ()
    model = HomeState
    view HomeState = Div [] []
