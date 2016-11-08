module Main exposing (..)

import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onBlur)
import Http
import Task exposing (Task, andThen)
import Process
import Time
import Json.Decode as Json exposing (object1, string, (:=))
import Plugins.SimplePlugin as SimplePlugin
import Plugins.AdvancedPlugin as AdvancedPlugin
import Plugins.PluginDispatcher as PluginDispatcher exposing (Plugin, PluginMessage)
import FormUtils
import Autocomplete


main =
    let
        model =
            { sender = "1"
            , receiver = "2"
            , currentPlugin = Nothing
            , formIsDisabled = True
            , autoState = Autocomplete.reset updateConfig Autocomplete.empty
            , showAutocomplete = False
            }
                |> updateCurrentPlugin
    in
        App.program
            { init = ( model, initialLookup )
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


doLookup : Task Http.Error (List String)
doLookup =
    Http.get places ("/data.json")


places : Json.Decoder (List String)
places =
    Json.list ("name" := Json.string)


introduceArtificialDelay : Task a b -> Task a b
introduceArtificialDelay task =
    Process.sleep (0.5 * Time.second)
        `andThen` \() -> task


initialLookup =
    Task.perform InitialFetchFail InitialFetchSucceed <| introduceArtificialDelay doLookup


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SetAutocompleteState Autocomplete.subscription ]



-- MODEL


type alias Model =
    { sender : String
    , receiver : String
    , currentPlugin : Maybe Plugin
    , formIsDisabled : Bool
    , autoState : Autocomplete.State
    , showAutocomplete : Bool
    }



-- UPDATE


type Msg
    = UpdateSender String
    | UpdateReceiver String
    | UpdatePlugin PluginMessage
    | InitialFetchSucceed (List String)
    | InitialFetchFail Http.Error
    | UpdateQuery String
    | SelectPerson String
    | SetAutocompleteState Autocomplete.Msg
    | Wrap Bool


updateCurrentPlugin : Model -> Model
updateCurrentPlugin model =
    let
        currentPlugin =
            PluginDispatcher.getPlugin (model.sender ++ model.receiver)
    in
        { model | currentPlugin = currentPlugin }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSender value ->
            updateCurrentPlugin { model | sender = value } ! []

        UpdateReceiver value ->
            updateCurrentPlugin { model | receiver = value } ! []

        UpdatePlugin message ->
            let
                plugin =
                    case model.currentPlugin of
                        Just model ->
                            Just (PluginDispatcher.update message model)

                        Nothing ->
                            Nothing
            in
                { model | currentPlugin = plugin } ! []

        UpdateQuery string ->
            let
                showAutocomplete =
                    case string of
                        "" ->
                            False

                        _ ->
                            True
            in
                { model | showAutocomplete = showAutocomplete } ! []

        InitialFetchSucceed results ->
            { model | formIsDisabled = False } ! []

        InitialFetchFail _ ->
            model ! []

        SelectPerson id ->
            model ! []

        SetAutocompleteState autoMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autoMsg howManyToShow model.autoState autocompleteEntries

                newModel =
                    { model | autoState = newState }
            in
                case maybeMsg of
                    Nothing ->
                        newModel ! []

                    Just updateMsg ->
                        update updateMsg newModel

        Wrap up ->
            let
                resetStrategy =
                    if up then
                        Autocomplete.resetToFirstItem
                    else
                        Autocomplete.resetToLastItem

                newAutoState =
                    resetStrategy updateConfig autocompleteEntries howManyToShow model.autoState
            in
                { model | autoState = newAutoState } ! []



-- VIEW


updateConfig : Autocomplete.UpdateConfig Msg Person
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 13 then
                    Maybe.map SelectPerson maybeId
                else
                    Nothing
        , onTooLow = Just (Wrap True)
        , onTooHigh = Just (Wrap False)
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectPerson id
        , separateSelections = False
        }


viewConfig : Autocomplete.ViewConfig Person
viewConfig =
    let
        customizedLi keySelected mouseSelected person =
            { attributes = [ classList [ ( "list-group-item", True ), ( "active", keySelected || mouseSelected ) ] ]
            , children = [ Html.text person.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul =
                [ class "list-group" ]
            , li =
                customizedLi
            }


type alias Person =
    { name : String }


produceOptions values current =
    List.map (\v -> option [ selected (current == v), value v ] [ text v ]) values


showCurrentPlugin : Model -> Html Msg
showCurrentPlugin model =
    let
        default =
            div [] [ text "no plugin available for this config. Try 1+2 or 2+1." ]
    in
        case model.currentPlugin of
            Just currentPlugin ->
                PluginDispatcher.view UpdatePlugin currentPlugin

            Nothing ->
                default


view : Model -> Html Msg
view model =
    let
        senderSelect =
            select [ class "form-control", on "change" (targetValue UpdateSender) ]
                (produceOptions [ "", "1", "2", "3" ] model.sender)

        receiverSelect =
            select [ class "form-control", on "change" (targetValue UpdateReceiver) ]
                (produceOptions [ "", "1", "2", "3" ] model.receiver)

        senderFormField =
            FormUtils.formField "Sender" senderSelect []

        receiverFormField =
            FormUtils.formField "Receiver" receiverSelect []

        someOtherInput =
            let
                autocompleteMenu =
                    if model.showAutocomplete then
                        div [ style [ ( "position", "absolute" ), ( "z-index", "1000" ) ] ]
                            [ App.map SetAutocompleteState (Autocomplete.view viewConfig howManyToShow model.autoState autocompleteEntries) ]
                    else
                        div [] []
            in
                div []
                    [ input [ type' "text", class "form-control", placeholder "...", onInput UpdateQuery, onBlur (UpdateQuery "") ] []
                    , autocompleteMenu
                    ]

        someOtherInputField =
            FormUtils.formField "Some Other Input" someOtherInput []

        yetAnotherInputField =
            let
                field =
                    input [ type' "text", class "form-control", placeholder "..." ] []
            in
                FormUtils.formField "Yet Another Input" field []

        rest =
            [ showCurrentPlugin model
            , someOtherInputField
            , yetAnotherInputField
            , stylesheet
            ]
    in
        Html.form [ class "form-horizontal" ]
            [ fieldset [ disabled model.formIsDisabled ]
                [ div []
                    ([ senderFormField, receiverFormField ] ++ rest)
                ]
            ]


targetValue : (String -> Msg) -> Json.Decoder Msg
targetValue msg =
    object1 msg ("target" := (object1 identity ("value" := string)))


stylesheet =
    let
        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            ]
    in
        node "link" attrs []


autocompleteEntries =
    [ { name = "abc" }
    , { name = "another one" }
    , { name = "even more!" }
    , { name = "one" }
    , { name = "two" }
    , { name = "three" }
    ]


howManyToShow =
    4
