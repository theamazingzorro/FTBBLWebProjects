module Api exposing (Endpoint(..), deleteRequest, getRequest, postRequest, putRequest)

import Env
import Http exposing (Body, Expect)
import Model.Coach as Coach exposing (CoachId)
import Model.Division as Div exposing (DivisionId)
import Model.Race as Race exposing (RaceId)
import Model.Team as Team exposing (TeamId)


type Endpoint
    = Teams
    | Team TeamId
    | Coaches
    | Coach CoachId
    | Races
    | Race RaceId
    | Divisions
    | Division DivisionId
    | Signin


baseUrl : String
baseUrl =
    Env.baseApiUrl


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

        Races ->
            "race"

        Race index ->
            "race/" ++ Race.idToString index

        Divisions ->
            "div"

        Division index ->
            "div/" ++ Div.idToString index

        Signin ->
            "signin"


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
