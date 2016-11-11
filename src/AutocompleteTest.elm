module AutocompleteTest exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Platform.Sub
import Json.Decode
import Task
import MyAutocomplete
import Array
import String
import DemoRestApi exposing (..)


main =
    Html.App.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init =
    { autocomplete = MyAutocomplete.init
    , anotherAutocomplete = MyAutocomplete.init
    , query = ""
    , anotherQuery = ""
    , selection = ""
    , wordList = []
    }


type alias Model =
    { autocomplete : MyAutocomplete.Model
    , anotherAutocomplete : MyAutocomplete.Model
    , query : String
    , selection : String
    , anotherQuery : String
    , wordList : List String
    }


type Msg
    = AutocompleteUpdate MyAutocomplete.Msg
    | AnotherAutocompleteUpdate MyAutocomplete.Msg
    | FetchDataForAutocomplete String
    | WordFetchSucceed (List String)
    | WordFetchFail Http.Error


update msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                availableOptions =
                    model.wordList

                ( maybeSelection, maybeQuery, autocomplete', autocompleteMessage ) =
                    MyAutocomplete.defaultUpdateBehaviour acmsg model.autocomplete availableOptions

                selection' =
                    Maybe.withDefault model.selection maybeSelection

                query' =
                    Maybe.withDefault model.query maybeQuery

                autocompleteMessage' =
                    [ Cmd.map AutocompleteUpdate autocompleteMessage ]

                fetchMsg =
                    case maybeQuery of
                        Just query ->
                            [ Task.perform FetchDataForAutocomplete FetchDataForAutocomplete (Task.succeed query) ]

                        _ ->
                            []
            in
                { model | autocomplete = autocomplete', selection = selection', query = query' } ! (autocompleteMessage' ++ fetchMsg)

        AnotherAutocompleteUpdate acmsg ->
            let
                availableOptions =
                    model.wordList

                ( maybeSelection, maybeQuery, autocomplete', autocompleteMessage' ) =
                    MyAutocomplete.defaultUpdateBehaviour acmsg model.anotherAutocomplete availableOptions

                query' =
                    Maybe.withDefault model.anotherQuery maybeQuery
            in
                { model | anotherAutocomplete = autocomplete', anotherQuery = query' } ! [ Cmd.map AnotherAutocompleteUpdate autocompleteMessage' ]

        FetchDataForAutocomplete query ->
            let
                cmd =
                    Task.perform WordFetchFail WordFetchSucceed <| fetchWords query
            in
                model ! [ cmd ]

        WordFetchSucceed wordList ->
            let
                a =
                    Debug.log "wordList" wordList
            in
                { model | wordList = wordList } ! []

        WordFetchFail error ->
            let
                a =
                    Debug.log "wordList" error
            in
                model ! []


view : Model -> Html Msg
view model =
    div [ class "form-horizontal" ]
        [ stylesheet
        , Html.App.map AutocompleteUpdate (MyAutocomplete.autocompleteableFormField model.wordList model.query "Le Field" model.autocomplete)
          -- , Html.App.map AnotherAutocompleteUpdate (MyAutocomplete.autocompleteableFormField model.wordList model.anotherQuery "Another query" model.anotherAutocomplete)
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
