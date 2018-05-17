module Test.Proxy where

import Pure.Data.Default
import Pure.Data.HTML
import Pure.Data.HTML.Properties
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.DOM

import Semantic.Proxy as Proxy

proxyTest =
  Proxy def <| InnerRef (\_ -> print "Outer") |>
    [ Proxy def <| InnerRef (\_ -> print "Inner") |>
        [ Div
        ]
    ]
