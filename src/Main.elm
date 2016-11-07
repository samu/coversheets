module Main exposing (..)

import Html exposing (..)
import Html.App as App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Http
import Task exposing (Task, andThen)
import Process
import Time
import Json.Decode as Json exposing (object1, string, (:=))
import Plugins.SimplePlugin as SimplePlugin
import Plugins.AdvancedPlugin as AdvancedPlugin
import Plugins.PluginDispatcher as PluginDispatcher exposing (Plugin, PluginMessage)


main =
    let
        model =
            Model "" "" "" "1" "1" Nothing |> updateCurrentPlugin
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
    Process.sleep (2 * Time.second)
        `andThen` \() -> task


initialLookup =
    Task.perform FetchFail FetchSucceed <| introduceArtificialDelay doLookup


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , sender : String
    , receiver : String
    , currentPlugin : Maybe Plugin
    }



-- UPDATE


type Msg
    = UpdateSender String
    | UpdateReceiver String
    | UpdatePlugin PluginMessage
    | FetchSucceed (List String)
    | FetchFail Http.Error


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

        FetchSucceed results ->
            let
                r =
                    Debug.log "results" results
            in
                model ! []

        FetchFail _ ->
            model ! []



-- VIEW


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
    div []
        [ select
            [ on "change" (targetValue UpdateSender) ]
            (produceOptions [ "", "1", "2", "3" ] model.sender)
        , select
            [ on "change" (targetValue UpdateReceiver) ]
            (produceOptions [ "", "1", "2", "3" ] model.receiver)
        , div []
            [ text model.sender, text model.receiver ]
        , showCurrentPlugin model
        ]


targetValue : (String -> Msg) -> Json.Decoder Msg
targetValue msg =
    object1 msg ("target" := (object1 identity ("value" := string)))
