-- {-# LANGUAGE OverloadedStrings #-}
-- module Main where

-- -- import Test.Shared

-- -- import Test.Button
-- -- import Test.Proxy
-- -- import Test.Header
-- -- import Test.Icon
-- -- import Test.Image

-- -- import Control.Monad (void)

-- import Pure.DOM
-- import Pure.Data.Lifted as L
-- import Pure.Data.View
-- import Pure.Data.View.Patterns
-- import Pure.Data.Txt
-- import Pure.Data.Default
-- import Pure.Data.HTML
-- import Pure.Data.HTML.Properties
-- import Pure.Data.Events

-- -- import Semantic.Dimmer

-- -- data Test = Test

-- -- instance Pure Test where
-- --   view = ComponentIO $ \self ->
-- --     let
-- --         toggle = void $ setState self $ \_ b -> do
-- --           print b
-- --           return (not b,return ())
-- --     in
-- --         def
-- --             { construct = return True
-- --             , render = \_ b ->
-- --                 b ? (Dimmer def <| Page True . Active b . Semantic.Dimmer.OnClick toggle |> [ A <| Pure.Data.Events.OnClickWith def { stopProp = True } (\_ -> toggle) |> [ "Close" ] ])
-- --                   $ (A <| Pure.Data.Events.OnClick (\_ -> toggle) |> [ "Open" ])
-- --             }


-- main = do
--   inject L.body (fromTxt "Hello, World!")
{-# LANGUAGE PatternSynonyms, RecordWildCards, FlexibleInstances #-}
module Main where

-- from pure-dom
import Pure.DOM

-- from pure-lifted
import Pure.Data.Lifted as L

-- from pure-core
import Pure.Data.View
import Pure.Data.View.Patterns

-- from pure-txt
import Pure.Data.Txt as Txt

-- from pure-default
import Pure.Data.Default

-- from pure-html
import Pure.Data.HTML as HTML
import Pure.Data.HTML.Properties

-- from pure-events
import Pure.Data.Events

-- from base
import Control.Arrow ((&&&))
import Control.Monad (void)
import qualified Data.List as L
import Data.Monoid ((<>))
import Data.Traversable (for)

-- from unordered-containers
import qualified Data.HashMap.Strict as HM

-- from vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

foreign import javascript unsafe
  "$r = Math.round(Math.random()*1000)%$1"
    rand :: Int -> IO Int

data Row = Row
  { ident    :: Int
  , label    :: Txt
  , selected :: Bool
  } deriving Eq

data Model = Model
  { rows    :: [Row]
  , lastId  :: Int
  , hashmap :: HM.HashMap Int Int
  }

adjectives :: V.Vector Txt
adjectives = V.fromList
  [  "pretty",  "large",  "big",  "small",  "tall",  "short",  "long"
  ,  "handsome",  "plain",  "quaint",  "clean",  "elegant",  "easy",  "angry"
  ,  "crazy",  "helpful",  "mushy",  "odd",  "unsightly",  "adorable"
  ,  "important",  "inexpensive",  "cheap",  "expensive",  "fancy"
  ]

colors :: V.Vector Txt
colors = V.fromList
  [  "red",  "yellow",  "blue",  "green",  "pink",  "brown"
  ,  "purple",  "brown",  "white",  "black",  "orange"
  ]

nouns :: V.Vector Txt
nouns = V.fromList
  [  "table",  "chair",  "house",  "bbq",  "desk",  "car"
  ,  "pony",  "cookie",  "sandwich",  "burger",  "pizza"
  ,  "mouse",  "keyboard"
  ]

choose :: V.Vector x -> IO x
choose vs = do
  r <- rand (V.length vs)
  return $! vs V.! r

createRows :: Int -> Int -> IO [Row]
createRows n largest = for [0..n] $ \x -> do
  adjective <- choose adjectives
  color     <- choose colors
  noun      <- choose nouns
  return Row
    { ident    = largest + x
    , label    = Txt.intercalate " " [adjective,color,noun]
    , selected = False
    }

data Msg
  = CreateM Int
  | AppendM Int
  | UpdateEveryM Int
  | ClearM
  | SwapM
  | RemoveM Int
  | SelectM Int

type Updater = Msg -> IO ()

update :: Msg -> Model -> IO Model
update msg mdl =
  case msg of
    CreateM amount -> do
      newRows <- createRows amount (lastId mdl)
      return mdl  { rows = newRows, lastId = lastId mdl + amount  }

    AppendM amount -> do
      newRows <- createRows amount (lastId mdl)
      return mdl { rows = rows mdl <> newRows, lastId = lastId mdl + amount }

    UpdateEveryM amount ->
      return mdl { rows = updateEvery amount bang (rows mdl) }

    ClearM ->
      return mdl { rows = [] }

    SwapM ->
      return mdl { rows = swap 1 998 (rows mdl) }

    RemoveM i ->
      return mdl { rows = removeIndex (i - 1) (rows mdl) }

    SelectM i ->
      return mdl { rows = updateOne (i - 1) (\(Row i l s) -> Row i l (not s)) (rows mdl) }

bang :: Row -> Row
bang row = row { label = label row <> " !!!" }

updateEvery :: Int -> (a -> a) -> [a] -> [a]
updateEvery n f = V.toList . update . V.fromList
  where
    update vector =
      let
        count = quot (V.length vector) n
        patch x = ( x * n, f )
        patches = V.generate count patch
      in
        V.accumulate (flip ($)) vector patches

updateOne :: Int -> (a -> a) -> [a] -> [a]
updateOne i f = V.toList . update . V.fromList
  where
    update = V.modify $ \v -> do
      r <- MV.unsafeRead v i
      MV.unsafeWrite v i (f r)

swap :: Int -> Int -> [a] -> [a]
swap i j = V.toList . swapped . V.fromList
  where
    swapped = V.modify (\v -> MV.swap v i j)

removeIndex :: Int -> [a] -> [a]
removeIndex i = uncurry (<>) . fmap (L.drop 1) . L.splitAt i

buttons :: [(Txt,Txt,Msg)]
buttons =
    [ ( "run10", "Create 10 rows", CreateM 10 )
    , ( "run", "Create 1,000 rows", CreateM 1000 )
    , ( "runlots", "Create 10,000 rows", CreateM 10000 )
    , ( "add10", "Append 100 rows", AppendM 10 )
    , ( "add", "Append 1,000 rows", AppendM 1000 )
    , ( "update2", "Update every 2nd row", UpdateEveryM 2 )
    , ( "update", "Update every 10th row", UpdateEveryM 10 )
    , ( "clear", "Clear", ClearM )
    , ( "swaprows", "Swap Rows", SwapM )
    ]

tbody :: [(Int,View)] -> View
tbody kcs = (Keyed Tbody) <||#> kcs

buttonPrimaryBlock :: Updater -> (Txt,Txt,Msg) -> View
buttonPrimaryBlock f (ident,label,msg) =
  Div <| Classes ["col-sm-6 smallpad"] |>
      [ Button <| Type "button" . Classes ["btn btn-primary btn-block"] . Id ident . OnClick (\_ -> f msg) . Attribute ("ref","text") |>
          [ fromTxt label ]
      ]

keyedRow :: Updater -> Row -> (Int,View)
keyedRow f = ident &&& row
  where
    row = ComponentIO $ \self ->
      let select _ = do
            r <- getProps self
            f (SelectM (ident r))

          remove _ = do
            r <- getProps self
            f (RemoveM (ident r))
      in
          def
              { construct = return ()
              , render = \r _ ->
                  Tr <| ((selected r) ? Classes ["danger"] $ id) |>
                      [ Td <| Classes ["col-md-1"] |> (fromTxt (toTxt (ident r)))
                      , Td <| Classes ["col-md-4"] |> [ A <| OnClick select |> (fromTxt (label r)) ]
                      , Td <| Classes ["col-md-1"] |>
                          [ A <| OnClick remove |>
                              [ HTML.Span <| Classes ["glyphicon glyphicon-remove"] . Property ("aria-hidden","true") ]
                          ]
                      , Td <| Classes ["col-md-6"]
                      ]
              }

main :: IO ()
main = Pure.DOM.body (View Main.Body)

data Body = Body
instance Pure Main.Body where
  view = ComponentIO $ \self ->
    let
        update msg = void $ setState self $ \_ mdl -> do
          mdl' <- Main.update msg mdl
          return (mdl',return ())
    in
        def
            { construct = return (Model [] 1 HM.empty)
            , render = \_ model ->
                Div <| Id "main" |>
                  [ Div <| Class "container" |>
                      [ Div <| Class "jumbotron" |>
                          [ Div <| Class "row" |>
                              [ Div <| Class "col-md-6" |>
                                  [ H1 <||> [ "pure-v0.7-keyed" ] ]
                              , Div <| Class "cold-md-6" |>
                                  ( L.map (buttonPrimaryBlock update) buttons )
                              ]
                          ]
                      , Table <| Class "table table-hover table-striped test-data" |>
                          [ tbody ( L.map (keyedRow update) (rows model) ) ]
                      , HTML.Span <| Class "preloadicon glyphicon glyphicon-remove" . Property ("aria-hidden","true")
                      ]
                  ]
            }
