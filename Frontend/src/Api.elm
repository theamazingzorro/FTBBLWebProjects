module Api exposing (Endpoint(..), getRequest)

import Http exposing (Expect)


type Endpoint
    = Teams
    | Team Int
    | Coaches
    | Coach Int


baseUrl : String
baseUrl =
    "https://localhost:17317/api/"


stringOf : Endpoint -> String
stringOf endpoint =
    case endpoint of
        Teams ->
            "team"

        Team index ->
            "team/" ++ String.fromInt index

        Coaches ->
            "coach"

        Coach index ->
            "coach/" ++ String.fromInt index


getRequest : Endpoint -> Expect msg -> Cmd msg
getRequest endpoint expect =
    Http.get
        { url = baseUrl ++ stringOf endpoint
        , expect = expect
        }
