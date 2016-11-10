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
    , query = ""
    , selection = ""
    }


getOptions model =
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
        List.filter (String.contains model.query) availableOptions


type alias Model =
    { autocomplete : MyAutocomplete.Model
    , query : String
    , selection : String
    }


type Msg
    = AutocompleteUpdate MyAutocomplete.Msg


getItemFromOptions idx model =
    let
        normalizedIdx =
            idx % (List.length (getOptions model))
    in
        case Array.fromList (getOptions model) |> Array.get normalizedIdx of
            Just item ->
                item

            Nothing ->
                "n/a"


defaultUpdateBehaviour acmsg model =
    let
        ( autocomplete, autocompleteMessage ) =
            MyAutocomplete.update acmsg model.autocomplete

        selection =
            case acmsg of
                MyAutocomplete.OnHoverExternal idx ->
                    Just (getItemFromOptions idx model)

                _ ->
                    Nothing

        query =
            case acmsg of
                -- MyAutocomplete.OnHoverExternal idx ->
                --     Just (getItemFromOptions idx model)
                MyAutocomplete.OnInputExternal query ->
                    Just query

                MyAutocomplete.OnPickExternal idx ->
                    Just (getItemFromOptions idx model)

                _ ->
                    Nothing
    in
        ( selection, query, autocomplete, autocompleteMessage )


update msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                ( maybeSelection, maybeQuery, autocomplete', autocompleteMessage' ) =
                    defaultUpdateBehaviour acmsg model

                selection' =
                    Maybe.withDefault model.selection maybeSelection

                query' =
                    Maybe.withDefault model.query maybeQuery
            in
                { model | autocomplete = autocomplete', selection = selection', query = query' } ! [ Cmd.map AutocompleteUpdate autocompleteMessage' ]


view : Model -> Html Msg
view model =
    div [ class "form-horizontal" ]
        [ stylesheet
        , Html.App.map AutocompleteUpdate (MyAutocomplete.autocompleteableFormField (getOptions model) model.query "Le Field" model.autocomplete)
          -- , Html.App.map AutocompleteUpdate (MyAutocomplete.autocompleteableFormField (getOptions model) model.query "Le Field" model.autocomplete)
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
