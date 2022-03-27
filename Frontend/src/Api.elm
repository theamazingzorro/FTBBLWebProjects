module Api exposing (Endpoint(..), deleteRequest, getRequest, postRequest, putRequest)

import Http exposing (Body, Expect)
import Model.Coach as Coach exposing (CoachId)
import Model.Team as Team exposing (TeamId)


type Endpoint
    = Teams
    | Team TeamId
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
            "team/" ++ Team.idToString index

        Coaches ->
            "coach"

        Coach index ->
            "coach/" ++ Coach.idToString index


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


putRequest : Endpoint -> Body -> Expect msg -> Cmd msg
putRequest endpoint body expect =
    Http.request
        { method = "PUT"
        , headers = []
        , url = urlOf endpoint
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
