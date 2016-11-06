module Plugins.SimplePlugin exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


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
    div []
        [ text "Document Type: "
        , input [ type' "text", placeholder "Enter a document type", onInput UpdateDocumentType ] []
        , text model.documentType
        ]
