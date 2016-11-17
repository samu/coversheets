module DebouncedAutocomplete exposing (Model, Msg, init, view, update)

import Html exposing (..)
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
    | WordFetch (Result Http.Error (List entity))


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

                ( maybeSelection, maybeQuery, autocomplete_, autocompleteMessage ) =
                    Autocomplete.defaultUpdateBehaviour acmsg model.autocomplete availableOptions

                query_ =
                    Maybe.withDefault model.query maybeQuery

                ( debouncedQuery_, debouncerMsg, _ ) =
                    debounceQueryChange maybeQuery model.debouncedQuery

                autocompleteMessage_ =
                    Cmd.map AutocompleteUpdate autocompleteMessage

                debouncerMsg_ =
                    Cmd.map DebounceUpdate debouncerMsg

                wordList_ =
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
                    | query = query_
                    , debouncedQuery = debouncedQuery_
                    , autocomplete = autocomplete_
                    , wordList = wordList_
                }
                    ! [ autocompleteMessage_, debouncerMsg_ ]

        DebounceUpdate debounceMsg ->
            let
                ( debouncedQuery_, debounceCmd, debounceMaybeQuery ) =
                    Debounce.update debounceMsg model.debouncedQuery

                fetchMsg =
                    case debounceMaybeQuery of
                        Just query ->
                            [ Task.attempt WordFetch <| listFetcher query ]

                        _ ->
                            []
            in
                { model
                    | debouncedQuery = debouncedQuery_
                }
                    ! ([ Cmd.map DebounceUpdate debounceCmd ]
                        ++ fetchMsg
                      )

        WordFetch (Ok wordList) ->
            { model | wordList = Debug.log "wordList" wordList } ! []

        WordFetch (Err _) ->
            model ! []


view : String -> EntityStringFetcher entity -> Model entity -> Html (Msg entity)
view label entityStringFetcher model =
    Html.map AutocompleteUpdate (Autocomplete.autocompleteableFormField model.wordList entityStringFetcher model.query label model.autocomplete)
