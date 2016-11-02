module Plugins.MoreAdvancedPlugin exposing (Model, Msg, init, view)

import Html exposing (..)


type alias Model =
    { creationDate : String
    , freeText : String
    }


type Msg
    = Nothing


init : Model
init =
    { creationDate = "abc"
    , freeText = "blabla"
    }


view : Model -> Html Msg
view model =
    div [] [ text "this is more advanced plugin taking over" ]
