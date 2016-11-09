
import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Platform.Sub
import Json.Decode
import Task

main = 
  Html.App.program 
    { init = init ! []
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
    
init = 
  { autocomplete = acinit
  }

-------------------
-- CORE PROGRAM 
-------------------

type alias Model =
  { autocomplete : AcModel
  }
  
    
type Msg
  = AutocompleteUpdate AcMsg


update msg model =
  case msg of
    AutocompleteUpdate acmsg ->
      let 
        (newAutocomplete, autocompleteMessage) = acupdate acmsg model.autocomplete
      in
        { model | autocomplete = newAutocomplete } ! [Cmd.map AutocompleteUpdate autocompleteMessage]

view : Model -> Html Msg
view model =
  div [ class "form-horizontal" ] 
    [ stylesheet
    , Html.App.map AutocompleteUpdate (autocompleteableFormField "Le Field" model.autocomplete)
    , text (toString model.autocomplete.currentPosition)
    ]
  
  
  
-------------------
-- Autocomplete Lib 
-------------------
type alias AcModel = 
  { show : Bool
  , currentPosition : Maybe Int
  }


type AcMsg
  = AcOnInput String
  | AcOnKeypress Int
  | AcOnMouseEnter Int
  | AcOnEscape
  
acinit = 
  { show = False
  , currentPosition = Nothing
  }


acupdate : AcMsg -> AcModel -> (AcModel, Cmd AcMsg)
acupdate msg model =
  case msg of
    AcOnInput string ->
      let 
        show = case string of 
           "" ->
             False
           _ ->
             True
             
        a = Debug.log "AcOnInput" 0
          
      in
        { model | show = show, currentPosition = Nothing } ! []
     
    AcOnKeypress keyCode ->
      let
        a = Debug.log "keyCode" keyCode
        
        b = Debug.log "AcOnKeypress" 0
        
        currentPosition' = 
          case (model.currentPosition, keyCode) of
            (Nothing, 38) ->
              Just -1
            (Nothing, 40) ->
              Just 0
            (Just int, 38) ->
              Just (int - 1)
            (Just int, 40) ->
              Just (int + 1)
            (_, _) ->
              model.currentPosition
         
        newMsg = 
          case keyCode of 
            27 ->
              [ Task.perform (always AcOnEscape) (always AcOnEscape) (Task.succeed Nothing)]
            _ ->
              []
      in 
        { model | currentPosition = currentPosition' } ! newMsg
    
    AcOnMouseEnter idx ->
      { model | currentPosition = Just idx } ! []
      
    AcOnEscape ->
      { model | currentPosition = Nothing, show = False } ! []



-- BOOTSTRAP HELPERS


inputClass = 
  class "form-control"


formField : String -> Html a -> List (Html a) -> Html a
formField label' input list =
    div [ class "form-group" ]
        ([ label [ for label', class "control-label col-sm-2" ]
            [ text label' ]
         , div [ class "col-sm-4" ]
            [ input ]
         ]
            ++ list
        )


autocompleteableFormField : String -> AcModel -> Html AcMsg
autocompleteableFormField name autocompleteModel =
  let
    {show} = 
      autocompleteModel
      
    options = 
      ["one", "two", "three", "bla", "blubb"]
    
    idx = 
      case autocompleteModel.currentPosition of
        Just int ->
          Just (int % (List.length options))
          
        Nothing ->
          Nothing

    suggestions = 
      if show then 
        listGroup options <| Debug.log "idx" idx
      else 
        div [] []
       
    
    onInput' = onInput AcOnInput
    
    onKeyDown' = on "keydown" (Json.Decode.map AcOnKeypress keyCode)

    html = 
      div [] 
        [ input [ type' "text", inputClass, onInput', onKeyDown' ] []
        , suggestions
        ]

  in
    formField name html []


listGroup options idx = 
  let
    renderActive = 
      case idx of
        Just int ->
          \n -> ("active", n == int)

        Nothing ->
          \n -> ("active", False)
    
    createOptionElement idx' value =       
      a [ onMouseEnter (AcOnMouseEnter idx'), classList [("list-group-item", True), renderActive idx'] ] [ text value ]
      
    options' = 
      List.indexedMap createOptionElement options
  in
    div [ class "list-group" ] options'
      

stylesheet =
    let
        attrs =
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
            ]
    in
        node "link" attrs []


