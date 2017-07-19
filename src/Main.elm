port module TodoApp exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Models exposing (..)
import Msgs exposing (..)
import Update exposing (update)
import View exposing (view)
import Html exposing (..)
import Json.Decode as Json
import Json.Encode as Encode
import Debug


main : Program (Maybe Encode.Value) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Encode.Value -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage (modelToValue newModel), cmds ]
        )



-- MODEL

init : Maybe Encode.Value -> ( Model, Cmd Msg )
init savedModel =
    case savedModel of 
    Just value ->
        Maybe.withDefault emptyModel (Json.decodeValue modelDecoder value |> resultToMaybe) 
        ! []
    Nothing -> emptyModel ! []

resultToMaybe : Result String Model -> Maybe Model
resultToMaybe result =
  case result of
    Result.Ok model -> Just model
    Result.Err error -> Debug.log error Nothing
