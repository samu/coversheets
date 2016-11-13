module DebouncedAutocomplete exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Time
import Task exposing (Task)
import FormUtils
import Autocomplete
import Debounce


type alias Model entity =
    { query : String
    , debouncedQuery : Debounce.Model String
    , wordList : List entity
    , autocomplete : Autocomplete.Model
    }


type Msg entity
    = AutocompleteUpdate Autocomplete.Msg
    | DebounceUpdate (Debounce.Msg String)
    | WordFetchSucceed (List entity)
    | WordFetchFail Http.Error


init : Model entity
init =
    { query = "blabla"
    , debouncedQuery = Debounce.init (Time.second * 0.5) ""
    , wordList = []
    , autocomplete = Autocomplete.init
    }


debounceQueryChange maybeQuery debouncedQuery =
    case maybeQuery of
        Just query ->
            Debounce.update (Debounce.Change query) debouncedQuery

        _ ->
            ( debouncedQuery, Cmd.none, Nothing )


type alias ListFetcher entity =
    String -> Task Http.Error (List entity)


type alias EntityStringFetcher entity =
    entity -> String


update : ListFetcher entity -> EntityStringFetcher entity -> Msg entity -> Model entity -> ( Model entity, Cmd (Msg entity) )
update listFetcher entityStringFetcher msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                availableOptions =
                    List.map entityStringFetcher model.wordList

                ( maybeSelection, maybeQuery, autocomplete', autocompleteMessage ) =
                    Autocomplete.defaultUpdateBehaviour acmsg model.autocomplete availableOptions

                query' =
                    Maybe.withDefault model.query maybeQuery

                ( debouncedQuery', debouncerMsg, _ ) =
                    debounceQueryChange maybeQuery model.debouncedQuery

                autocompleteMessage' =
                    Cmd.map AutocompleteUpdate autocompleteMessage

                debouncerMsg' =
                    Cmd.map DebounceUpdate debouncerMsg

                wordList' =
                    case maybeQuery of
                        Just query ->
                            if query == "" then
                                []
                            else
                                model.wordList

                        _ ->
                            model.wordList
            in
                { model
                    | query = query'
                    , debouncedQuery = debouncedQuery'
                    , autocomplete = autocomplete'
                    , wordList = wordList'
                }
                    ! [ autocompleteMessage', debouncerMsg' ]

        DebounceUpdate debounceMsg ->
            let
                ( debouncedQuery', debounceCmd, debounceMaybeQuery ) =
                    Debounce.update debounceMsg model.debouncedQuery

                fetchMsg =
                    case debounceMaybeQuery of
                        Just query ->
                            [ Task.perform WordFetchFail WordFetchSucceed <| listFetcher query ]

                        _ ->
                            []
            in
                { model
                    | debouncedQuery = debouncedQuery'
                }
                    ! ([ Cmd.map DebounceUpdate debounceCmd ]
                        ++ fetchMsg
                      )

        WordFetchSucceed wordList ->
            { model | wordList = Debug.log "wordList" wordList } ! []

        WordFetchFail error ->
            model ! []


view : String -> EntityStringFetcher entity -> Model entity -> Html (Msg entity)
view label entityStringFetcher model =
    App.map AutocompleteUpdate (Autocomplete.autocompleteableFormField model.wordList entityStringFetcher model.query label model.autocomplete)
