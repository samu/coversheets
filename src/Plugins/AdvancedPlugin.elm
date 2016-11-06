module Plugins.AdvancedPlugin exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { creationDate : String
    , freeText : String
    }


type Msg
    = Update String


init : Model
init =
    { creationDate = "abc"
    , freeText = "blabla"
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Update text ->
            { model | freeText = text }


view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", placeholder "Name", onInput Update ] []
        , text model.freeText
        ]
