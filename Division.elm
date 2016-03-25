module Division where

import Html exposing (..)
import Html.Attributes exposing (id, class, property, value, type', size)
import Json.Encode as Json
import Effects
import Random

import Multiplication exposing (Action, showParams, split, problems, Problem)

showProblem : Problem -> Html
showProblem (a, b) =
  div [class "problem"]
      [ text (toString <| a * b)
      , span [property "innerHTML" <| Json.string " &divide; "] []
      , text (toString a)
      , text " = "]

saveState : Multiplication.Model -> ( Multiplication.Model, Effects.Effects Multiplication.Action )
saveState = Multiplication.saveState

initialModel = Multiplication.initialModel

update = Multiplication.update

deserializeModel = Multiplication.deserializeModel

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

modelSignal = Multiplication.modelSignal

type alias Model = Multiplication.Model
