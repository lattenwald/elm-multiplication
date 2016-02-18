module Main where

import Html exposing (..)
import Html.Attributes exposing (id, class, property)
import Random
import Json.Encode as Json

-- types
type alias Problem = (Int, Int)

-- settings
cols : Int
cols = 2

rows : Int
rows = 25

minA : Int
minA = 2

minB : Int
minB = 2

maxA : Int
maxA = 10

maxB : Int
maxB = 10

intSeed : Int
intSeed = 1234

-- functions
problems : Int -> Random.Generator (List Problem)
problems quantity =
  let genA = Random.int minA maxA
      genB = Random.int minB maxB
      genPair = Random.pair genA genB
  in Random.list quantity genPair

showProblem : Problem -> Html
showProblem (a, b) =
  div [class "problem"]
      [ text (toString a)
      , span [property "innerHTML" <| Json.string " &times; "] []
      , text (toString b)
      , text " = "]

-- split : Int -> List a -> List (List a)
split n ls =
  let
    rest : List a
    rest = List.drop n ls
  in if List.isEmpty rest
     then [ls]
     else List.take n ls :: split n rest

showParams : Html
showParams = p []
  [ text "seed = ", text <| toString intSeed
  , text ", a = ", text <| toString minA, text " to ", text <| toString maxA
  , text ", b = ", text <| toString minB, text " to ", text <| toString maxB
  , text ", table of ", text <| toString cols, text "x", text <| toString rows ]

main : Html
main =
  let seed = Random.initialSeed intSeed
      (prob, _) = Random.generate (problems <| cols * rows) seed
      prob' = split cols prob
  in div [id "content"] <|
       [ table [] <|
               List.map (\row -> tr [] <|
                           List.map (\p -> td [] [showProblem p]) row) prob'
       , showParams ]

-- port initialLocation : String -- incoming
-- port location : Signal String -- outgoing

-- textContent : Channel Content
-- textContent = channel {noContent | string <- initialLocation}
