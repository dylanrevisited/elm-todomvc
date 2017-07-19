module Update exposing (..)

import Models exposing (..)
import Msgs exposing (..)
import Dom
import String
import Task


-- UPDATE
-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Add ->
            { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [ newEntry model.field model.uid ]
            }
                ! []

        UpdateField str ->
            { model | field = str }
                ! []

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }
                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ toString id)
            in
                { model | entries = List.map updateEntry model.entries }
                    ! [ Task.attempt (\_ -> NoOp) focus ]

        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | description = task }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        Delete id ->
            { model | entries = List.filter (\t -> t.id /= id) model.entries }
                ! []

        DeleteComplete ->
            { model | entries = List.filter (not << .completed) model.entries }
                ! []

        Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }
                    else
                        t
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
                { model | entries = List.map updateEntry model.entries }
                    ! []

        ChangeVisibility visibility ->
            { model | visibility = visibility }
                ! []
