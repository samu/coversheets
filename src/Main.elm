module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing (object1, string, (:=))
import Plugins.SimplePlugin as SimplePlugin
import Plugins.MoreAdvancedPlugin as MoreAdvancedPlugin
import Plugins.PluginDispatcher as PluginDispatcher exposing (Plugin, PluginMessage)


main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , sender : String
    , receiver : String
    , currentPlugin : Maybe Plugin
    }


model : Model
model =
    Model "" "" "" "" "" Nothing



-- UPDATE


type Msg
    = UpdateSender String
    | UpdateReceiver String
    | UpdatePlugin PluginMessage


updateCurrentPlugin : Model -> Model
updateCurrentPlugin model =
    let
        currentPlugin =
            PluginDispatcher.getPlugin (model.sender ++ model.receiver)
    in
        { model | currentPlugin = currentPlugin }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateSender value ->
            updateCurrentPlugin { model | sender = value }

        UpdateReceiver value ->
            updateCurrentPlugin { model | receiver = value }

        UpdatePlugin message ->
            model



-- VIEW


produceOptions values current =
    List.map (\v -> option [ selected (current == v), value v ] [ text v ]) values


showCurrentPlugin : Model -> Html Msg
showCurrentPlugin model =
    let
        default =
            div [] [ text "nothing to show, boring :(" ]
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
