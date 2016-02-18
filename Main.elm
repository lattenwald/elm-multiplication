import StartApp.Simple exposing (start)
import Multiplication exposing (..)

main = start { model = initialModel, update = update, view = view }
