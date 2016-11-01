module SimplePlugin exposing (Model, Msg, init, view)
import Html exposing (..)

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
    model

view : Model -> Html Msg
view model =
    div [] [ text "this is simple plugin speaking" ]
