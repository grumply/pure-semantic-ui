module Main where

import Pure.DOM
import Pure.Data.Lifted
import Pure.Data.View
import Pure.Data.View.Patterns
import Pure.Data.Default
import Pure.Data.Txt

import Semantic.Modal as Modal

main = do
  b <- getBody
  inject (Element $ toJSV b) (Modal def <| DimmerType (Just "blurring") . Trigger (fromTxt "Test") |> [ fromTxt "Hello"])
