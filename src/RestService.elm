module RestService exposing (fetchWords, fetchPeople, Word, Person)

import Json.Decode as Json exposing (string, (:=))
import Process
import Task exposing (Task, andThen)
import Http
import Time
import String


introduceArtificialDelay : Task a b -> Task a b
introduceArtificialDelay task =
    Process.sleep (Time.second * 0.1)
        `andThen` \() -> task


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


type alias Person =
    { name : String
    , email : String
    }


fetchPeople : String -> Task Http.Error (List Person)
fetchPeople query =
    let
        httpTask =
            if query == "" then
                Task.succeed []
            else
                introduceArtificialDelay (Http.get peopleDecoder ("/people.json"))

        filterResults result =
            Task.succeed (List.filter (\n -> String.contains query n.name) result)
    in
        httpTask `andThen` filterResults


peopleDecoder : Json.Decoder (List Person)
peopleDecoder =
    Json.list <| Json.object2 Person ("name" := Json.string) ("email" := Json.string)
