module Plugins.SimplePlugin exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import FormUtils


type alias Model =
    { documentType : String
    , documentVersion : String
    }


type Msg
    = UpdateDocumentType String
    | UpdateDocumentVersion String


init : Model
init =
    { documentType = ""
    , documentVersion = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateDocumentType text ->
            { model | documentType = text }

        _ ->
            model


view : Model -> Html Msg
view model =
    let
        inputField =
            input [ type_ "text", class "form-control", placeholder "Enter a document type", onInput UpdateDocumentType ] []

        plaintextLabel =
            text model.documentType
    in
        FormUtils.formField "Document Type" inputField [ plaintextLabel ]
