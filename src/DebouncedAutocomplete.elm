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


type alias Model =
    { query : String
    , debouncedQuery : Debounce.Model String
    , wordList : List String
    , autocomplete : Autocomplete.Model
    }


type Msg
    = AutocompleteUpdate Autocomplete.Msg
    | DebounceUpdate (Debounce.Msg String)
    | WordFetchSucceed (List String)
    | WordFetchFail Http.Error


init : Model
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


type alias ListFetcher =
    String -> Task Http.Error (List String)


update : ListFetcher -> Msg -> Model -> ( Model, Cmd Msg )
update listFetcher msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                ( maybeSelection, maybeQuery, autocomplete', autocompleteMessage ) =
                    Autocomplete.defaultUpdateBehaviour acmsg model.autocomplete model.wordList

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


view : String -> Model -> Html Msg
view label model =
    App.map AutocompleteUpdate (Autocomplete.autocompleteableFormField model.wordList model.query label model.autocomplete)
