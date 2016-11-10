module MyAutocomplete exposing (Model, Msg(OnInputExternal, OnHoverExternal, OnPickExternal), init, update, defaultUpdateBehaviour, autocompleteableFormField)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Sub
import Json.Decode
import Task
import Array


type alias Model =
    { show : Bool
    , currentPosition : Maybe Int
    }


type Msg
    = OnInput String
    | OnKeypress Int
    | OnMouseEnter Int
    | OnClick Int
    | OnEscape
    | OnEnter Int
    | OnBlur
    | OnInputExternal String
    | OnHoverExternal Int
    | OnPickExternal Int


init =
    { show = False
    , currentPosition = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInput string ->
            let
                show =
                    case string of
                        "" ->
                            False

                        _ ->
                            True

                a =
                    Debug.log "OnInput" 0

                msg =
                    Task.perform OnInputExternal OnInputExternal (Task.succeed string)
            in
                { model | show = show, currentPosition = Nothing } ! [ msg ]

        OnKeypress keyCode ->
            let
                a =
                    Debug.log "keyCode" keyCode

                b =
                    Debug.log "OnKeypress" 0

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
                            [ Task.perform (always OnEscape) (always OnEscape) (Task.succeed Nothing) ]

                        _ ->
                            []

                onEnterMsg =
                    case currentPosition' of
                        Just idx ->
                            case keyCode of
                                13 ->
                                    [ Task.perform OnEnter OnEnter (Task.succeed idx) ]

                                _ ->
                                    []

                        Nothing ->
                            []

                onHoverMsg =
                    case currentPosition' of
                        Just idx ->
                            if keyCode == 38 || keyCode == 40 then
                                [ Task.perform OnHoverExternal OnHoverExternal (Task.succeed idx) ]
                            else
                                []

                        Nothing ->
                            []
            in
                { model | currentPosition = currentPosition' } ! (onEscapeMsg ++ onHoverMsg ++ onEnterMsg)

        OnMouseEnter idx ->
            let
                msg =
                    [ Task.perform OnHoverExternal OnHoverExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Just idx } ! msg

        OnClick idx ->
            let
                msg =
                    [ Task.perform OnPickExternal OnPickExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Nothing, show = False } ! msg

        OnEscape ->
            { model | currentPosition = Nothing, show = False } ! []

        OnEnter idx ->
            let
                msg =
                    [ Task.perform OnPickExternal OnPickExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Nothing, show = False } ! msg

        OnBlur ->
            { model | currentPosition = Nothing, show = False } ! []

        OnInputExternal string ->
            model ! []

        OnHoverExternal idx ->
            model ! []

        OnPickExternal idx ->
            model ! []


getItemFromOptions idx availableOptions =
    let
        normalizedIdx =
            idx % (List.length (availableOptions))

        maybeItem =
            Array.fromList availableOptions |> Array.get normalizedIdx
    in
        Maybe.withDefault "n/a" maybeItem


defaultUpdateBehaviour acmsg acmodel availableOptions =
    let
        ( autocomplete, autocompleteMessage ) =
            update acmsg acmodel

        selection =
            case acmsg of
                OnHoverExternal idx ->
                    Just (getItemFromOptions idx availableOptions)

                _ ->
                    Nothing

        query =
            case acmsg of
                -- OnHoverExternal idx ->
                --     Just (getItemFromOptions idx query)
                OnInputExternal query' ->
                    Just query'

                OnPickExternal idx ->
                    Just (getItemFromOptions idx availableOptions)

                _ ->
                    Nothing
    in
        ( selection, query, autocomplete, autocompleteMessage )


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
            onInput OnInput

        onKeyDown' =
            on "keydown" (Json.Decode.map OnKeypress keyCode)

        onBlur' =
            onBlur OnBlur

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
            a [ onMouseEnter (OnMouseEnter idx'), onClick (OnClick idx'), classList [ ( "list-group-item", True ), renderActive idx' ] ] [ text value ]

        options' =
            List.indexedMap createOptionElement options
    in
        div [ class "list-group", style [ ( "position", "absolute" ), ( "z-index", "1000" ) ] ] options'
