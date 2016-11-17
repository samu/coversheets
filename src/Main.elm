module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onBlur)
import Http exposing (Request)
import Task exposing (Task, andThen)
import Process
import Time
import Json.Decode as Json exposing (string, field)
import Plugins.SimplePlugin as SimplePlugin
import Plugins.AdvancedPlugin as AdvancedPlugin
import Plugins.PluginDispatcher as PluginDispatcher exposing (Plugin, PluginMessage)
import FormUtils


main =
    let
        model =
            { sender = "2"
            , receiver = "1"
            , currentPlugin = Nothing
            , formIsDisabled = True
            , query = ""
            }
                |> updateCurrentPlugin
    in
        Html.program
            { init = ( model, initialLookup )
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


doLookup : Task Http.Error (List String)
doLookup =
    Http.toTask (Http.get "/data.json" places)


places : Json.Decoder (List String)
places =
    Json.list (field "name" Json.string)


introduceArtificialDelay : Task a b -> Task a b
introduceArtificialDelay task =
    Process.sleep (0.5 * Time.second)
        |> andThen (\() -> task)


initialLookup =
    Task.attempt InitialFetch <| introduceArtificialDelay doLookup


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- MODEL


type alias Model =
    { sender : String
    , receiver : String
    , currentPlugin : Maybe Plugin
    , formIsDisabled : Bool
    , query : String
    }



-- UPDATE


type Msg
    = UpdateSender String
    | UpdateReceiver String
    | UpdatePlugin PluginMessage
    | InitialFetch (Result Http.Error (List String))
    | UpdateQuery String


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
                ( plugin, pluginMessage ) =
                    case model.currentPlugin of
                        Just model ->
                            let
                                ( plugin_, pluginMessage_ ) =
                                    PluginDispatcher.update message model
                            in
                                ( Just plugin_, Cmd.map UpdatePlugin pluginMessage_ )

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
                { model | currentPlugin = plugin } ! [ pluginMessage ]

        UpdateQuery string ->
            { model | query = string } ! []

        InitialFetch (Ok results) ->
            { model | formIsDisabled = False } ! []

        InitialFetch (Err _) ->
            { model | formIsDisabled = False } ! []



-- VIEW


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


produceOptions values current =
    List.map (\v -> option [ selected (current == v), value v ] [ text v ]) values


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
            div []
                [ input [ type_ "text", value model.query, class "form-control", placeholder "...", onInput UpdateQuery ] []
                ]

        someOtherInputField =
            FormUtils.formField "Some Other Input" someOtherInput []

        yetAnotherInputField =
            let
                field =
                    input [ type_ "text", class "form-control", placeholder "..." ] []
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
    Json.map msg (field "target" (Json.map identity (field "value" string)))


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
