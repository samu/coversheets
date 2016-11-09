module MyAutocomplete exposing (Model, Msg(AcOnInputExternal, AcOnSelectionExternal), init, update, autocompleteableFormField)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Sub
import Json.Decode
import Task


type alias Model =
    { show : Bool
    , currentPosition : Maybe Int
    }


type Msg
    = AcOnInput String
    | AcOnInputExternal String
    | AcOnSelectionExternal Int
    | AcOnKeypress Int
    | AcOnMouseEnter Int
    | AcOnEscape
    | AcOnBlur


init =
    { show = False
    , currentPosition = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AcOnInput string ->
            let
                show =
                    case string of
                        "" ->
                            False

                        _ ->
                            True

                a =
                    Debug.log "AcOnInput" 0

                msg =
                    Task.perform AcOnInputExternal AcOnInputExternal (Task.succeed string)
            in
                { model | show = show, currentPosition = Nothing } ! [ msg ]

        AcOnInputExternal string ->
            model ! []

        AcOnSelectionExternal idx ->
            model ! []

        AcOnKeypress keyCode ->
            let
                a =
                    Debug.log "keyCode" keyCode

                b =
                    Debug.log "AcOnKeypress" 0

                currentPosition' =
                    case ( model.currentPosition, keyCode ) of
                        ( Nothing, 38 ) ->
                            Just -1

                        ( Nothing, 40 ) ->
                            Just 0

                        ( Just int, 38 ) ->
                            Just (int - 1)

                        ( Just int, 40 ) ->
                            Just (int + 1)

                        ( _, _ ) ->
                            model.currentPosition

                onEscapeMsg =
                    case keyCode of
                        27 ->
                            [ Task.perform (always AcOnEscape) (always AcOnEscape) (Task.succeed Nothing) ]

                        _ ->
                            []

                onSelectionMsg =
                    case model.currentPosition of
                        Just int ->
                            if keyCode == 38 || keyCode == 40 then
                                [ Task.perform AcOnSelectionExternal AcOnSelectionExternal (Task.succeed int) ]
                            else
                                []

                        Nothing ->
                            []
            in
                { model | currentPosition = currentPosition' } ! (onEscapeMsg ++ onSelectionMsg)

        AcOnMouseEnter idx ->
            { model | currentPosition = Just idx } ! []

        AcOnEscape ->
            { model | currentPosition = Nothing, show = False } ! []

        AcOnBlur ->
            { model | currentPosition = Nothing, show = False } ! []


inputClass =
    class "form-control"


formField : String -> Html a -> List (Html a) -> Html a
formField label' input list =
    div [ class "form-group" ]
        ([ label [ for label', class "control-label col-sm-2" ]
            [ text label' ]
         , div [ class "col-sm-4" ]
            [ input ]
         ]
            ++ list
        )


autocompleteableFormField : String -> Model -> Html Msg
autocompleteableFormField name autocompleteModel =
    let
        { show } =
            autocompleteModel

        options =
            [ "one", "two", "three", "bla", "blubb" ]

        idx =
            case autocompleteModel.currentPosition of
                Just int ->
                    Just (int % (List.length options))

                Nothing ->
                    Nothing

        suggestions =
            if show then
                listGroup options idx
            else
                div [] []

        onInput' =
            onInput AcOnInput

        onKeyDown' =
            on "keydown" (Json.Decode.map AcOnKeypress keyCode)

        onBlur' =
            onBlur AcOnBlur

        html =
            div []
                [ input [ type' "text", inputClass, onInput', onKeyDown', onBlur' ] []
                , suggestions
                ]
    in
        formField name html []


listGroup options idx =
    let
        renderActive =
            case idx of
                Just int ->
                    \n -> ( "active", n == int )

                Nothing ->
                    \n -> ( "active", False )

        createOptionElement idx' value =
            a [ onMouseEnter (AcOnMouseEnter idx'), classList [ ( "list-group-item", True ), renderActive idx' ] ] [ text value ]

        options' =
            List.indexedMap createOptionElement options
    in
        div [ class "list-group", style [ ( "position", "absolute" ) ] ] options'
