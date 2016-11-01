module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing (object1, string, (:=))


main =
    App.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type PluginData
    = DocumentTypeAndVersionPlugin
        { documentType : String
        , documentVersion : String
        }
    | RealEstatesPlugin
        { creationDate : String
        , freeText : String
        }


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
    Model "" "" "" "1" "2" Nothing



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Sender String
    | Receiver String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password }

        PasswordAgain password ->
            { model | passwordAgain = password }

        Sender value ->
            { model | sender = value }

        Receiver value ->
            { model | receiver = value }



-- VIEW


produceOptions values current =
    List.map (\v -> option [ selected (current == v), value v ] [ text v ]) values


view : Model -> Html Msg
view model =
    div []
        [ select
            [ on "change" (targetValue Sender) ]
            (produceOptions [ "1", "2", "3" ] model.sender)
        , select
            [ on "change" (targetValue Receiver) ]
            (produceOptions [ "1", "2", "3" ] model.receiver)
        , div []
            [ text model.sender, text model.receiver ]
        ]


targetValue : (String -> Msg) -> Json.Decoder Msg
targetValue msg =
    object1 msg ("target" := (object1 identity ("value" := string)))
