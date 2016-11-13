module Plugins.AdvancedPlugin exposing (Model, Msg, init, view, update)

import Html exposing (Html)
import Html.App as App
import Http
import DebouncedAutocomplete
import RestService exposing (fetchWords)


type alias Model =
    { query : String
    , debouncedAutocomplete : DebouncedAutocomplete.Model Entity
    }


type alias Entity =
    String


type Msg
    = DebouncedAutocompleteUpdate (DebouncedAutocomplete.Msg Entity)


init : Model
init =
    { query = "blabla"
    , debouncedAutocomplete = DebouncedAutocomplete.init
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebouncedAutocompleteUpdate dacmsg ->
            let
                ( debouncedAutocomplete', debouncedAutocompleteMsg ) =
                    DebouncedAutocomplete.update fetchWords identity dacmsg model.debouncedAutocomplete
            in
                { model
                    | debouncedAutocomplete = debouncedAutocomplete'
                }
                    ! [ Cmd.map DebouncedAutocompleteUpdate debouncedAutocompleteMsg ]


view : Model -> Html Msg
view model =
    App.map DebouncedAutocompleteUpdate (DebouncedAutocomplete.view "Word" identity model.debouncedAutocomplete)
