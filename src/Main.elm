module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing (object1, string, (:=))
import SimplePlugin
import MoreAdvancedPlugin


main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type PluginData
    = SimplePlugin SimplePlugin.Model
    | MoreAdvancedPlugin MoreAdvancedPlugin.Model


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , sender : String
    , receiver : String
    , pluginData : Maybe PluginData
    }


model : Model
model =
    Model "" "" "" "" "" Nothing



-- UPDATE


type Msg
    = UpdateSender String
    | UpdateReceiver String
    | UpdatePlugin SimplePlugin.Msg


updateCurrentPlugin : Model -> Model
updateCurrentPlugin model =
    let
        pluginData =
            case model.sender ++ model.receiver of
                "12" ->
                    Just (SimplePlugin SimplePlugin.init)

                _ ->
                    Nothing
    in
        { model | pluginData = pluginData }


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
            div [] []
    in
        case model.pluginData of
            Just pluginData ->
                case pluginData of
                    SimplePlugin data ->
                        App.map UpdatePlugin (SimplePlugin.view data)

                    _ ->
                        default

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
