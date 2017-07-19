module Models exposing (..)

import Json.Decode as Json
import Json.Encode as Encode


type Visibility
    = ShowAll
    | ShowCompleted
    | ShowActive



-- The full application state of our todo app.


type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    , visibility : Visibility
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }



-- Decoders


modelDecoder : Json.Decoder Model
modelDecoder =
    Json.map4 Model
        (Json.at [ "entries" ] (Json.list entryDecoder))
        (Json.at [ "field" ] Json.string)
        (Json.at [ "uid" ] Json.int)
        (Json.at [ "visibility" ] Json.string |> Json.andThen visibilityDecoder)


entryDecoder : Json.Decoder Entry
entryDecoder =
    Json.map4 Entry
        (Json.at [ "description" ] Json.string)
        (Json.at [ "completed" ] Json.bool)
        (Json.at [ "editing" ] Json.bool)
        (Json.at [ "id" ] Json.int)


visibilityDecoder : String -> Json.Decoder Visibility
visibilityDecoder tag =
    case tag of
        "ShowActive" ->
            Json.succeed ShowActive

        "ShowAll" ->
            Json.succeed ShowAll

        "ShowCompleted" ->
            Json.succeed ShowCompleted

        _ ->
            Json.fail (tag ++ " is not a recognized tag for Visibility")



-- Encoders


modelToValue : Model -> Encode.Value
modelToValue model =
    Encode.object
        [ ( "entries", Encode.list (List.map entryToValue model.entries) )
        , ( "field", Encode.string model.field )
        , ( "uid", Encode.int model.uid )
        , ( "visibility", visibilityToValue model.visibility )
        ]


entryToValue : Entry -> Encode.Value
entryToValue entry =
    Encode.object
        [ ( "description", Encode.string entry.description )
        , ( "completed", Encode.bool entry.completed )
        , ( "editing", Encode.bool entry.editing )
        , ( "id", Encode.int entry.id )
        ]


visibilityToValue : Visibility -> Encode.Value
visibilityToValue visibility =
    case visibility of
        ShowActive ->
            Encode.string "ShowActive"

        ShowAll ->
            Encode.string "ShowAll"

        ShowCompleted ->
            Encode.string "ShowCompleted"


emptyModel : Model
emptyModel =
    { entries = []
    , visibility = ShowAll
    , field = ""
    , uid = 0
    }


newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }
