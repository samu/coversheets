module Autocomplete exposing (Model, Msg(OnInputExternal, OnHoverExternal, OnPickExternal), init, update, defaultUpdateBehaviour, autocompleteableFormField)

import Html exposing (..)
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

                msg =
                    Task.perform OnInputExternal (Task.succeed string)
            in
                { model | show = show, currentPosition = Nothing } ! [ msg ]

        OnKeypress keyCode ->
            let
                currentPosition_ =
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
                            [ Task.perform (always OnEscape) (Task.succeed Nothing) ]

                        _ ->
                            []

                onEnterMsg =
                    case currentPosition_ of
                        Just idx ->
                            case keyCode of
                                13 ->
                                    [ Task.perform OnEnter (Task.succeed idx) ]

                                _ ->
                                    []

                        Nothing ->
                            []

                onHoverMsg =
                    case currentPosition_ of
                        Just idx ->
                            if keyCode == 38 || keyCode == 40 then
                                [ Task.perform OnHoverExternal (Task.succeed idx) ]
                            else
                                []

                        Nothing ->
                            []
            in
                { model | currentPosition = currentPosition_ } ! (onEscapeMsg ++ onHoverMsg ++ onEnterMsg)

        OnMouseEnter idx ->
            let
                msg =
                    [ Task.perform OnHoverExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Just idx } ! msg

        OnClick idx ->
            let
                msg =
                    [ Task.perform OnPickExternal (Task.succeed idx) ]
            in
                { model | currentPosition = Nothing, show = False } ! msg

        OnEscape ->
            { model | currentPosition = Nothing, show = False } ! []

        OnEnter idx ->
            let
                msg =
                    [ Task.perform OnPickExternal (Task.succeed idx) ]
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
                OnInputExternal query_ ->
                    Just query_

                OnPickExternal idx ->
                    Just (getItemFromOptions idx availableOptions)

                _ ->
                    Nothing
    in
        ( selection, query, autocomplete, autocompleteMessage )


inputClass =
    class "form-control"


formField : String -> Html a -> List (Html a) -> Html a
formField label_ input list =
    div [ class "form-group" ]
        ([ label [ for label_, class "control-label col-sm-2" ]
            [ text label_ ]
         , div [ class "col-sm-4" ]
            [ input ]
         ]
            ++ list
        )


autocompleteableFormField : List entity -> (entity -> String) -> String -> String -> Model -> Html Msg
autocompleteableFormField options entityToString query name autocompleteModel =
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
                listGroup options entityToString idx
            else
                div [] []

        onInput_ =
            onInput OnInput

        onKeyDown_ =
            on "keydown" (Json.Decode.map OnKeypress keyCode)

        onBlur_ =
            onBlur OnBlur

        html =
            div []
                [ input [ type_ "text", value query, inputClass, onInput_, onKeyDown_, onBlur_ ] []
                , suggestions
                ]
    in
        formField name html []


listGroup options entityToString idx =
    let
        renderActive =
            case idx of
                Just int ->
                    \n -> ( "active", n == int )

                Nothing ->
                    \n -> ( "active", False )

        createOptionElement idx_ value =
            a [ onMouseEnter (OnMouseEnter idx_), onClick (OnClick idx_), classList [ ( "list-group-item", True ), renderActive idx_ ] ] [ text (entityToString value) ]

        options_ =
            List.indexedMap createOptionElement options
    in
        div [ class "list-group", style [ ( "position", "absolute" ), ( "z-index", "1000" ) ] ] options_
