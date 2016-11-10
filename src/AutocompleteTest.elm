module AutocompleteTest exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Sub
import Json.Decode
import Task
import MyAutocomplete
import Array
import String


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
    }


filteredOptions query =
    let
        availableOptions =
            [ "stream"
            , "porter"
            , "convulsion"
            , "fascinate"
            , "excavate"
            , "pollution"
            , "likely"
            , "snake"
            , "present"
            , "truck"
            , "dribble"
            , "cabin"
            , "reveal"
            , "available"
            , "roar"
            , "manual"
            , "chauvinist"
            , "still"
            , "ice"
            , "fool"
            ]
    in
        List.filter (String.contains query) availableOptions


type alias Model =
    { autocomplete : MyAutocomplete.Model
    , anotherAutocomplete : MyAutocomplete.Model
    , query : String
    , selection : String
    , anotherQuery : String
    }


type Msg
    = AutocompleteUpdate MyAutocomplete.Msg
    | AnotherAutocompleteUpdate MyAutocomplete.Msg


update msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                availableOptions =
                    filteredOptions model.query

                ( maybeSelection, maybeQuery, autocomplete', autocompleteMessage' ) =
                    MyAutocomplete.defaultUpdateBehaviour acmsg model.autocomplete availableOptions

                selection' =
                    Maybe.withDefault model.selection maybeSelection

                query' =
                    Maybe.withDefault model.query maybeQuery
            in
                { model | autocomplete = autocomplete', selection = selection', query = query' } ! [ Cmd.map AutocompleteUpdate autocompleteMessage' ]

        AnotherAutocompleteUpdate acmsg ->
            let
                availableOptions =
                    filteredOptions model.anotherQuery

                ( maybeSelection, maybeQuery, autocomplete', autocompleteMessage' ) =
                    MyAutocomplete.defaultUpdateBehaviour acmsg model.anotherAutocomplete availableOptions

                query' =
                    Maybe.withDefault model.anotherQuery maybeQuery
            in
                { model | anotherAutocomplete = autocomplete', anotherQuery = query' } ! [ Cmd.map AnotherAutocompleteUpdate autocompleteMessage' ]


view : Model -> Html Msg
view model =
    div [ class "form-horizontal" ]
        [ stylesheet
        , Html.App.map AutocompleteUpdate (MyAutocomplete.autocompleteableFormField (filteredOptions model.query) model.query "Le Field" model.autocomplete)
        , Html.App.map AnotherAutocompleteUpdate (MyAutocomplete.autocompleteableFormField (filteredOptions model.anotherQuery) model.anotherQuery "Another query" model.anotherAutocomplete)
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
