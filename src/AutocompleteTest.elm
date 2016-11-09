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


main =
    Html.App.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init =
    { autocomplete = MyAutocomplete.init
    , query = ""
    , leField = "le value"
    }


options =
    [ "one", "two", "three", "bla", "blubb" ]


type alias Model =
    { autocomplete : MyAutocomplete.Model
    , query : String
    , leField : String
    }


type Msg
    = AutocompleteUpdate MyAutocomplete.Msg


getItemFromOptions idx =
    let
        normalizedIdx =
            idx % (List.length options)
    in
        case Array.fromList options |> Array.get normalizedIdx of
            Just item ->
                item

            Nothing ->
                "n/a"


update msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                ( newAutocomplete, autocompleteMessage ) =
                    MyAutocomplete.update acmsg model.autocomplete

                leField' =
                    case acmsg of
                        MyAutocomplete.AcOnSelectionExternal idx ->
                            getItemFromOptions idx

                        _ ->
                            model.leField

                query' =
                    case acmsg of
                        MyAutocomplete.AcOnSelectionExternal idx ->
                            getItemFromOptions idx

                        MyAutocomplete.AcOnInputExternal query ->
                            query

                        _ ->
                            model.query
            in
                { model | autocomplete = newAutocomplete, leField = leField', query = query' } ! [ Cmd.map AutocompleteUpdate autocompleteMessage ]


view : Model -> Html Msg
view model =
    div [ class "form-horizontal" ]
        [ stylesheet
        , Html.App.map AutocompleteUpdate (MyAutocomplete.autocompleteableFormField options model.query "Le Field" model.autocomplete)
        , div [] [ text (toString model.autocomplete.currentPosition) ]
        , div [] [ text model.leField ]
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
