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

-- types
{-| @docs problem type -}
type alias Problem = (Int, Int)

type alias Model = {
    intSeed : Int
  , minA : Int
  , maxA : Int
  , minB : Int
  , maxB : Int
  , rows : Int
  , cols : Int
  }

initialModel : Model
initialModel = { intSeed = 1234
               , minA = 2
               , maxA = 10
               , minB = 2
               , maxB = 10
               , rows = 25
               , cols = 2 }

type Action = SetSeed Int | SetMinA Int | SetMinB Int | SetMaxA Int | SetMaxB Int

update : Action -> Model -> Model
update action model =
  case action of
    SetSeed s -> { model | intSeed = s }
    SetMinA a -> { model | minA = a }
    SetMaxA a -> { model | maxA = a }
    SetMinB b -> { model | minB = b }
    SetMaxB b -> { model | maxB = b }

view : Signal.Address Action -> Model -> Html
view address model =
  let seed = Random.initialSeed model.intSeed
      (prob, _) = Random.generate (problems model) seed
      prob' = split model.cols prob
  in div [id "content"] <|
         [ table [] <|
                 List.map (\row -> tr [] <|
                           List.map (\p -> td [] [showProblem p]) row) prob'
         , showParams model ]

-- functions
{-| @docs problems generator -}
problems : Model -> Random.Generator (List Problem)
problems model =
  let genA = Random.int model.minA model.maxA
      genB = Random.int model.minB model.maxB
      genPair = Random.pair genA genB
      quantity = model.rows * model.cols
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
showParams : Model -> Html
showParams model = p []
  [ text "seed = ", text <| toString model.intSeed
  , text ", a = ", text <| toString model.minA, text " to ", text <| toString model.maxA
  , text ", b = ", text <| toString model.minB, text " to ", text <| toString model.maxB
  , text ", table of ", text <| toString model.cols, text "x", text <| toString model.rows ]
