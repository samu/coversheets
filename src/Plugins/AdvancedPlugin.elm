module Plugins.AdvancedPlugin exposing (Model, Msg, init, view, update)

import Html exposing (Html, div)
import Http
import DebouncedAutocomplete
import RestService exposing (fetchWords, fetchPeople)


type alias Model =
    { debouncedAutocompleteForWord : DebouncedAutocomplete.Model Word
    , debouncedAutocompleteForPerson : DebouncedAutocomplete.Model Person
    }


type alias Word =
    RestService.Word


type alias Person =
    RestService.Person


wordToString =
    .word


personToString =
    .name


type Msg
    = DebouncedAutocompleteForWordUpdate (DebouncedAutocomplete.Msg Word)
    | DebouncedAutocompleteForPersonUpdate (DebouncedAutocomplete.Msg Person)


init : Model
init =
    { debouncedAutocompleteForWord = DebouncedAutocomplete.init
    , debouncedAutocompleteForPerson = DebouncedAutocomplete.init
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebouncedAutocompleteForWordUpdate dacmsg ->
            let
                ( debouncedAutocompleteForWord_, debouncedAutocompleteMsg ) =
                    DebouncedAutocomplete.update fetchWords wordToString dacmsg model.debouncedAutocompleteForWord
            in
                { model
                    | debouncedAutocompleteForWord = debouncedAutocompleteForWord_
                }
                    ! [ Cmd.map DebouncedAutocompleteForWordUpdate debouncedAutocompleteMsg ]

        DebouncedAutocompleteForPersonUpdate dacmsg ->
            let
                ( debouncedAutocompleteForPerson_, debouncedAutocompleteMsg ) =
                    DebouncedAutocomplete.update fetchPeople personToString dacmsg model.debouncedAutocompleteForPerson
            in
                { model
                    | debouncedAutocompleteForPerson = debouncedAutocompleteForPerson_
                }
                    ! [ Cmd.map DebouncedAutocompleteForPersonUpdate debouncedAutocompleteMsg ]


view : Model -> Html Msg
view model =
    div []
        [ Html.map DebouncedAutocompleteForWordUpdate (DebouncedAutocomplete.view "Word" wordToString model.debouncedAutocompleteForWord)
        , Html.map DebouncedAutocompleteForPersonUpdate (DebouncedAutocomplete.view "Person" personToString model.debouncedAutocompleteForPerson)
        ]
