module RestService exposing (fetchWords)

import Json.Decode as Json exposing (string)
import Process
import Task exposing (Task, andThen)
import Http
import Time
import String


fetchWords : String -> Task Http.Error (List String)
fetchWords query =
    let
        httpTask =
            introduceArtificialDelay (Http.get wordsDecoder ("/words.json"))

        filterResults result =
            Task.succeed (List.filter (String.contains query) result)
    in
        httpTask `andThen` filterResults


wordsDecoder : Json.Decoder (List String)
wordsDecoder =
    Json.list Json.string


introduceArtificialDelay : Task a b -> Task a b
introduceArtificialDelay task =
    Process.sleep (0.5 * Time.second)
        `andThen` \() -> task
