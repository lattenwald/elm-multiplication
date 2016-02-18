module Multiplication where

{-| Multiplication

# Types
@docs Problem

# Table format
@docs cols, rows

# Example bounds
@docs minA, minB, maxA, maxB

# RNG seed
@docs intSeed

# functions
@docs problems, showParams, showProblem, split, main

-}

import Html exposing (..)
import Html.Attributes exposing (id, class, property)
import Random
import Json.Encode as Json
import Signal exposing (..)

-- types
{-| @docs problem type -}
type alias Problem = (Int, Int)

-- settings
{-| @docs number of columns in examples table -}
cols : Int
cols = 2

{-| @docs number of rows in examples table -}
rows : Int
rows = 25

{-| @docs minimum A -}
minA : Int
minA = 2

{-| @docs mimunum B -}
minB : Int
minB = 2

{-| @docs maximum A -}
maxA : Int
maxA = 10

{-| @docs maximum B -}
maxB : Int
maxB = 10

{-| @docs RNG seed -}
intSeed : Int
intSeed = 1234

-- port initialLocation : String -- incoming
-- port location : Signal String -- outgoing

-- functions
{-| @docs problems generator -}
problems : Int -> Random.Generator (List Problem)
problems quantity =
  let genA = Random.int minA maxA
      genB = Random.int minB maxB
      genPair = Random.pair genA genB
  in Random.list quantity genPair

{-| @docs render problem -}
showProblem : Problem -> Html
showProblem (a, b) =
  div [class "problem"]
      [ text (toString a)
      , span [property "innerHTML" <| Json.string " &times; "] []
      , text (toString b)
      , text " = "]

{-| @docs split list -}
split : Int -> List a -> List (List a)
split n ls =
  let
    rest = List.drop n ls
  in if List.isEmpty rest
     then [ls]
     else List.take n ls :: split n rest

{-| @docs render params -}
showParams : Html
showParams = p []
  [ text "seed = ", text <| toString intSeed
  , text ", a = ", text <| toString minA, text " to ", text <| toString maxA
  , text ", b = ", text <| toString minB, text " to ", text <| toString maxB
  , text ", table of ", text <| toString cols, text "x", text <| toString rows ]

{-| @docs main render function -}
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
