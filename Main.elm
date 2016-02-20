import StartApp exposing (start, App)
import Multiplication exposing (..)
import Html exposing (Html)
import Effects exposing (Never)
import Task

app : App Model
app = start { init   = saveState (Maybe.withDefault initialModel <| deserializeModel initialHash)
            , update = update
            , view   = view
            , inputs = [ modelSignal initialModel ] }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

port initialHash : String
