module RestService exposing (fetchWords, fetchPeople, Word, Person)

import Json.Decode as Json exposing (string, field)
import Process
import Task exposing (Task, andThen)
import Http
import Time
import String


introduceArtificialDelay : Task a b -> Task a b
introduceArtificialDelay task =
    Process.sleep (Time.second * 0.1)
        |> andThen (\() -> task)


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
                introduceArtificialDelay <| Http.toTask (Http.get "/words.json" wordsDecoder)

        filterResults result =
            Task.succeed (List.filter (\n -> String.contains query n.word) result)
    in
        httpTask |> andThen filterResults


wordsDecoder : Json.Decoder (List Word)
wordsDecoder =
    Json.list <| Json.map Word (field "word" Json.string)


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
                introduceArtificialDelay <| Http.toTask (Http.get "/people.json" peopleDecoder)

        filterResults result =
            Task.succeed (List.filter (\n -> String.contains query n.name) result)
    in
        httpTask |> andThen filterResults


peopleDecoder : Json.Decoder (List Person)
peopleDecoder =
    Json.list <| Json.map2 Person (field "name" Json.string) (field "email" Json.string)
