module Multiplication where

import Html exposing (..)
import Html.Attributes exposing (id, class, property, value, type')
import Html.Events exposing (targetValue, on)
import Json.Encode as Json
import Random
import String
import Effects exposing (Effects)
import History
import Parser exposing ((<*), (*>), and)
import Parser.Number as Parser
import Debug

-- types
type alias Problem = (Int, Int)

type alias Model = {
    cols    : Int
  , rows    : Int
  , minA    : Int
  , maxA    : Int
  , minB    : Int
  , maxB    : Int
  , intSeed : Int
  }

initialModel : Model
initialModel = { cols = 2
               , rows = 25
               , minA = 2
               , maxA = 10
               , minB = 2
               , maxB = 10
               , intSeed = 1234 }

type Action = NoOp
            | SetSeed Int
            | SetMinA Int
            | SetMinB Int
            | SetMaxA Int
            | SetMaxB Int
            | SetRows Int
            | SetCols Int
            | LoadState Model

serializeModel : Model -> String
serializeModel model =
     toString model.cols ++ "x" ++ toString model.rows
  ++ "/" ++ toString model.minA ++ ".." ++ toString model.maxA
  ++ "/" ++ toString model.minB ++ ".." ++ toString model.maxB
  ++ "/" ++ toString model.intSeed

deserializeModel : String -> Maybe Model
deserializeModel str =
  let modelParser = Parser.optional (Parser.symbol '#') '#' *>
                    Parser.map Model
                    Parser.natural <* Parser.symbol 'x' `and`
                    Parser.natural <* Parser.symbol '/' `and`
                    Parser.integer <* Parser.token ".." `and`
                    Parser.integer <* Parser.token "/" `and`
                    Parser.integer <* Parser.token ".." `and`
                    Parser.integer <* Parser.token "/" `and`
                    Parser.integer
  in str |> Debug.log "model string" >> Parser.parse modelParser >> Result.toMaybe >> Debug.log "model parse result"

modelSignal : Model -> Signal Action
modelSignal model = Signal.map (deserializeModel >> Maybe.withDefault model >> LoadState) History.hash

saveState : Model -> (Model, Effects Action)
saveState model = ( model, "#" ++ serializeModel model |> History.setPath >> Effects.task >> Effects.map (\_ -> NoOp))

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp        -> (model, Effects.none)
    SetSeed s   -> saveState { model | intSeed = s }
    SetMinA a   -> saveState { model | minA = a }
    SetMaxA a   -> saveState { model | maxA = a }
    SetMinB b   -> saveState { model | minB = b }
    SetMaxB b   -> saveState { model | maxB = b }
    SetRows r   -> saveState { model | rows = r }
    SetCols c   -> saveState { model | cols = c }
    LoadState m -> (m, Effects.none)

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
              , type' "number"
              , on "input" targetValue (makeMessage f def address) ] []
  in p []
       [ text "seed = ", inp SetSeed model.intSeed, br [] []
       , text "a = ", inp SetMinA model.minA
       , text " to " , inp SetMaxA model.maxA
       , text ", b = ", inp SetMinB model.minB
       , text " to ", inp SetMaxB model.maxB
       , text ", table of ", inp SetCols model.cols
       , text "x", inp SetRows model.rows ]
