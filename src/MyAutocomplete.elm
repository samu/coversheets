module MyAutocomplete exposing (Model, Msg(AcOnInputExternal, AcOnHoverExternal, AcOnPickExternal), init, update, autocompleteableFormField)

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
    | AcOnHoverExternal Int
    | AcOnPickExternal Int
    | AcOnKeypress Int
    | AcOnMouseEnter Int
    | AcOnClick Int
    | AcOnEscape
    | AcOnEnter Int
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

        AcOnHoverExternal idx ->
            model ! []

        AcOnPickExternal idx ->
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

                onEnterMsg =
                    case currentPosition' of
                        Just idx ->
                            case keyCode of
                                13 ->
                                    [ Task.perform AcOnEnter AcOnEnter (Task.succeed idx) ]

                                _ ->
                                    []

                        Nothing ->
                            []

                onHoverMsg =
                    case currentPosition' of
                        Just idx ->
                            if keyCode == 38 || keyCode == 40 then
                                [ Task.perform AcOnHoverExternal AcOnHoverExternal (Task.succeed idx) ]
                            else
                                []

                        Nothing ->
                            []
            in
                { model | currentPosition = currentPosition' } ! (onEscapeMsg ++ onHoverMsg ++ onEnterMsg)

        AcOnMouseEnter idx ->
            let
                msg =
                    [ Task.perform AcOnHoverExternal AcOnHoverExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Just idx } ! msg

        AcOnClick idx ->
            let
                msg =
                    [ Task.perform AcOnPickExternal AcOnPickExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Nothing, show = False } ! msg

        AcOnEscape ->
            { model | currentPosition = Nothing, show = False } ! []

        AcOnEnter idx ->
            let
                msg =
                    [ Task.perform AcOnPickExternal AcOnPickExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Nothing, show = False } ! msg

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


autocompleteableFormField : List String -> String -> String -> Model -> Html Msg
autocompleteableFormField options query name autocompleteModel =
    let
        { show } =
            autocompleteModel

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
                [ input [ type' "text", value query, inputClass, onInput', onKeyDown', onBlur' ] []
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
            a [ onMouseEnter (AcOnMouseEnter idx'), onClick (AcOnClick idx'), classList [ ( "list-group-item", True ), renderActive idx' ] ] [ text value ]

        options' =
            List.indexedMap createOptionElement options
    in
        div [ class "list-group", style [ ( "position", "absolute" ) ] ] options'
