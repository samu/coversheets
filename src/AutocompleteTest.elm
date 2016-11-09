module AutocompleteTest exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Sub
import Json.Decode
import Task
import MyAutocomplete


main =
    Html.App.program
        { init = init ! []
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init =
    { autocomplete = MyAutocomplete.init
    , leField = "le value"
    }


type alias Model =
    { autocomplete : MyAutocomplete.Model
    , leField : String
    }


type Msg
    = AutocompleteUpdate MyAutocomplete.Msg


update msg model =
    case msg of
        AutocompleteUpdate acmsg ->
            let
                ( newAutocomplete, autocompleteMessage ) =
                    MyAutocomplete.update acmsg model.autocomplete

                leField' =
                    case acmsg of
                        MyAutocomplete.AcOnInputExternal value ->
                            value

                        _ ->
                            model.leField
            in
                { model | autocomplete = newAutocomplete, leField = leField' } ! [ Cmd.map AutocompleteUpdate autocompleteMessage ]


view : Model -> Html Msg
view model =
    div [ class "form-horizontal" ]
        [ stylesheet
        , Html.App.map AutocompleteUpdate (MyAutocomplete.autocompleteableFormField "Le Field" model.autocomplete)
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
