module Api exposing (Endpoint(..), deleteRequest, getRequest, postRequest)

import Http exposing (Body, Expect)
import Model.Coach exposing (CoachId, idToString)


type Endpoint
    = Teams
    | Team Int
    | Coaches
    | Coach CoachId


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
            "coach/" ++ idToString index


urlOf : Endpoint -> String
urlOf endpoint =
    baseUrl ++ stringOf endpoint


getRequest : Endpoint -> Expect msg -> Cmd msg
getRequest endpoint expect =
    Http.get
        { url = urlOf endpoint
        , expect = expect
        }


postRequest : Endpoint -> Body -> Expect msg -> Cmd msg
postRequest endpoint body expect =
    Http.post
        { url = urlOf endpoint
        , body = body
        , expect = expect
        }


deleteRequest : Endpoint -> Expect msg -> Cmd msg
deleteRequest endpoint expect =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = urlOf endpoint
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
