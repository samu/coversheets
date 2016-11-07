module Plugins.AdvancedPlugin exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import FormUtils


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
    let
        nameInput =
            input [ type' "text", class "form-control", placeholder "Name", onInput Update ] []

        formField =
            FormUtils.formField "Name" nameInput [ text model.freeText ]
    in
        formField
