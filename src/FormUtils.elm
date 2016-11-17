module FormUtils exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


formField : String -> Html a -> List (Html a) -> Html a
formField label_ input list =
    div [ class "form-group" ]
        ([ label [ for label_, class "control-label col-sm-2" ]
            [ text label_ ]
         , div [ class "col-sm-4" ]
            [ input ]
         ]
            ++ list
        )
