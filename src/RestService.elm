module RestService exposing (fetchWords, Word)

import Json.Decode as Json exposing (string, (:=))
import Process
import Task exposing (Task, andThen)
import Http
import Time
import String


type alias Word =
    { word : String
    }


fetchWords : String -> Task Http.Error (List Word)
fetchWords query =
    let
        httpTask =
            if query == "" then
                Task.succeed []
            else
                introduceArtificialDelay (Http.get wordsDecoder ("/words.json"))

        filterResults result =
            Task.succeed (List.filter (\n -> String.contains query n.word) result)
    in
        httpTask `andThen` filterResults


wordsDecoder : Json.Decoder (List Word)
wordsDecoder =
    Json.list <| Json.object1 Word ("word" := Json.string)


introduceArtificialDelay : Task a b -> Task a b
introduceArtificialDelay task =
    Process.sleep (Time.second * 0.1)
        `andThen` \() -> task
