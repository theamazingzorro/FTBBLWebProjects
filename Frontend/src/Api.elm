module Api exposing (Endpoint(..), getRequest, postRequest)

import Http exposing (Body, Expect)


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


postRequest : Endpoint -> Body -> Expect msg -> Cmd msg
postRequest endpoint body expect =
    Http.post
        { url = baseUrl ++ stringOf endpoint
        , body = body
        , expect = expect
        }
