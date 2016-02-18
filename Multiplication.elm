module Multiplication where

import Html exposing (..)
import Html.Attributes exposing (id, class, property, value)
import Html.Events exposing (targetValue, on)
import Json.Encode as Json
import Random
import String

-- types
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

type Action = SetSeed Int | SetMinA Int | SetMinB Int | SetMaxA Int | SetMaxB Int | SetRows Int | SetCols Int

update : Action -> Model -> Model
update action model =
  case action of
    SetSeed s -> { model | intSeed = s }
    SetMinA a -> { model | minA = a }
    SetMaxA a -> { model | maxA = a }
    SetMinB b -> { model | minB = b }
    SetMaxB b -> { model | maxB = b }
    SetRows r -> { model | rows = r }
    SetCols c -> { model | cols = c }

view : Signal.Address Action -> Model -> Html
view address model =
  let seed = Random.initialSeed model.intSeed
      (prob, _) = Random.generate (problems model) seed
      prob' = split model.cols prob
  in div [id "content"] <|
         [ table [] <|
                 List.map (\row -> tr [] <|
                           List.map (\p -> td [] [showProblem p]) row) prob'
         , showParams address model ]

-- functions
problems : Model -> Random.Generator (List Problem)
problems model =
  let genA = Random.int model.minA model.maxA
      genB = Random.int model.minB model.maxB
      genPair = Random.pair genA genB
      quantity = model.rows * model.cols
  in Random.list quantity genPair

showProblem : Problem -> Html
showProblem (a, b) =
  div [class "problem"]
      [ text (toString a)
      , span [property "innerHTML" <| Json.string " &times; "] []
      , text (toString b)
      , text " = "]

split : Int -> List a -> List (List a)
split n ls =
  let
    rest = List.drop n ls
  in if List.isEmpty rest
     then [ls]
     else List.take n ls :: split n rest

showParams : Signal.Address Action -> Model -> Html
showParams address model =
  let makeMessage f def str =
        String.toInt >> Result.toMaybe >> Maybe.withDefault def >> f >> Signal.message address
      inp f def =
        input [ value (toString def)
              , on "input" targetValue (makeMessage f def address) ] []
  in p []
       [ text "seed = ", inp SetSeed model.intSeed, br [] []
       , text "a = ", inp SetMinA model.minA
       , text " to " , inp SetMaxA model.maxA
       , text ", b = ", inp SetMinB model.minB
       , text " to ", inp SetMaxB model.maxB
       , text ", table of ", inp SetCols model.cols
       , text "x", inp SetRows model.rows ]
