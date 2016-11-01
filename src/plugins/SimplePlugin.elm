module SimplePlugin exposing (Model, init, view)
import Html exposing (..)

type alias Model =
    { documentType : String
    , documentVersion : String
    }

init : Model
init =
    { documentType = ""
    , documentVersion = ""
    }

view : Model -> Html a
view model =
    div [] [ text "this is simple plugin speaking" ]
