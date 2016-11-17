module AutocompleteTest exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Platform.Sub
import Json.Decode
import Task
import Autocomplete
import Array
import String
import RestService exposing (..)
import Debounce
import Time


main =
    Html.App.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init =
    { autocomplete = Autocomplete.init
    , anotherAutocomplete = Autocomplete.init
    , query = ""
    , debouncedQuery = Debounce.init (Time.millisecond * 500) ""
    , anotherQuery = ""
    , selection = ""
    , wordList = []
    }


type alias Model =
    { autocomplete : Autocomplete.Model
    , anotherAutocomplete : Autocomplete.Model
    , query : String
    , debouncedQuery : Debounce.Model String
    , selection : String
    , anotherQuery : String
    , wordList : List String
    }


type Msg
    = AutocompleteUpdate Autocomplete.Msg
    | AnotherAutocompleteUpdate Autocomplete.Msg
    | FetchDataForAutocomplete String
    | WordFetchSucceed (List String)
    | WordFetchFail Http.Error
    | DebounceMsg (Debounce.Msg String)


update msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                availableOptions =
                    model.wordList

                ( maybeSelection, maybeQuery, autocomplete_, autocompleteMessage ) =
                    Autocomplete.defaultUpdateBehaviour acmsg model.autocomplete availableOptions

                selection_ =
                    Maybe.withDefault model.selection maybeSelection

                query_ =
                    Maybe.withDefault model.query maybeQuery

                autocompleteMessage_ =
                    [ Cmd.map AutocompleteUpdate autocompleteMessage ]

                ( debouncedQuery_, debouncerMsg, _ ) =
                    case maybeQuery of
                        Just query ->
                            Debounce.update (Debounce.Change query) model.debouncedQuery

                        _ ->
                            ( model.debouncedQuery, Cmd.none, Nothing )

                debouncerMsg_ =
                    [ Cmd.map DebounceMsg debouncerMsg ]

                wordList_ =
                    case maybeQuery of
                        Just query ->
                            []

                        Nothing ->
                            model.wordList
            in
                { model | autocomplete = autocomplete_, selection = selection_, query = query_, debouncedQuery = debouncedQuery_, wordList = wordList_ } ! (autocompleteMessage_ ++ debouncerMsg_)

        AnotherAutocompleteUpdate acmsg ->
            let
                availableOptions =
                    model.wordList

                ( maybeSelection, maybeQuery, autocomplete_, autocompleteMessage_ ) =
                    Autocomplete.defaultUpdateBehaviour acmsg model.anotherAutocomplete availableOptions

                query_ =
                    Maybe.withDefault model.anotherQuery maybeQuery
            in
                { model | anotherAutocomplete = autocomplete_, anotherQuery = query_ } ! [ Cmd.map AnotherAutocompleteUpdate autocompleteMessage_ ]

        FetchDataForAutocomplete query ->
            let
                cmd =
                    Task.perform WordFetchFail WordFetchSucceed <| fetchWords query
            in
                model ! [ cmd ]

        WordFetchSucceed wordList ->
            { model | wordList = wordList } ! []

        WordFetchFail error ->
            model ! []

        DebounceMsg debounceMsg ->
            let
                ( debouncedQuery_, debounceCmd, debounceMaybeQuery ) =
                    Debounce.update debounceMsg model.debouncedQuery

                fetchMsg =
                    case debounceMaybeQuery of
                        Just query ->
                            [ Task.perform FetchDataForAutocomplete FetchDataForAutocomplete (Task.succeed query) ]

                        _ ->
                            []
            in
                { model | debouncedQuery = debouncedQuery_ } ! ([ Cmd.map DebounceMsg debounceCmd ] ++ fetchMsg)


view : Model -> Html Msg
view model =
    div [ class "form-horizontal" ]
        [ stylesheet
        , Html.App.map AutocompleteUpdate (Autocomplete.autocompleteableFormField model.wordList model.query "Le Field" model.autocomplete)
          -- , Html.App.map AnotherAutocompleteUpdate (Autocomplete.autocompleteableFormField model.wordList model.anotherQuery "Another query" model.anotherAutocomplete)
          -- , div [] [ text (toString model.autocomplete.currentPosition) ]
        , div [] [ text "selection: ", text model.selection ]
        ]


stylesheet =
    let
        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            ]
    in
        node "link" attrs []
