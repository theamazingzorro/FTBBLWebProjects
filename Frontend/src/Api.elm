module Api exposing (Endpoint(..), deleteRequest, getRequest, postRequest, putRequest)

import Env
import Http exposing (Body, Expect, Header(..))
import Model.Accolade as Accolade exposing (AccoladeId)
import Model.Coach as Coach exposing (CoachId)
import Model.Division as Div exposing (DivisionId)
import Model.Game as Game exposing (GameId)
import Model.Race as Race exposing (RaceId)
import Model.Team as Team exposing (TeamId)


type Endpoint
    = Teams
    | FreeTeams
    | TeamsInDiv DivisionId
    | TeamsNotInDiv DivisionId
    | TeamUpdateDiv TeamId DivisionId
    | Team TeamId
    | Coaches
    | Coach CoachId
    | Races
    | Race RaceId
    | Divisions
    | Division DivisionId
    | CloseDivision DivisionId
    | Games
    | GamesInDiv DivisionId
    | Game GameId
    | Accolades
    | Accolade AccoladeId
    | DivStandings DivisionId
    | TeamStanding DivisionId TeamId
    | Signin
    | TeamEloHistory TeamId
    | CoachEloHistory CoachId


baseUrl : String
baseUrl =
    Env.baseApiUrl


stringOf : Endpoint -> String
stringOf endpoint =
    case endpoint of
        Teams ->
            "team"

        FreeTeams ->
            "team/free/"

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

        Games ->
            "game/"

        Game index ->
            "game/" ++ Game.idToString index

        Accolades ->
            "accolade/"

        Accolade index ->
            "accolade/" ++ Accolade.idToString index

        Signin ->
            "signin"

        GamesInDiv index ->
            "game/bydiv/" ++ Div.idToString index

        TeamsInDiv index ->
            "team/bydiv/" ++ Div.idToString index

        TeamsNotInDiv index ->
            "team/notindiv/" ++ Div.idToString index

        TeamUpdateDiv teamId divId ->
            "team/updatediv/" ++ Team.idToString teamId ++ "/" ++ Div.idToString divId

        CloseDivision divId ->
            "div/close/" ++ Div.idToString divId

        DivStandings divId ->
            "standings/" ++ Div.idToString divId

        TeamStanding divId teamId ->
            "standings/" ++ Div.idToString divId ++ "/" ++ Team.idToString teamId

        TeamEloHistory teamId ->
            "history/team/" ++ Team.idToString teamId

        CoachEloHistory coachId ->
            "history/coach/" ++ Coach.idToString coachId


urlOf : Endpoint -> String
urlOf endpoint =
    baseUrl ++ stringOf endpoint


getHeaders : Maybe String -> List Header
getHeaders token =
    [ Http.header "Authorization" <| "Bearer " ++ Maybe.withDefault "" token ]


getRequest : Maybe String -> Endpoint -> Expect msg -> Cmd msg
getRequest token endpoint expect =
    Http.request
        { method = "GET"
        , headers = getHeaders token
        , url = urlOf endpoint
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


postRequest : Maybe String -> Endpoint -> Body -> Expect msg -> Cmd msg
postRequest token endpoint body expect =
    Http.request
        { method = "POST"
        , headers = getHeaders token
        , url = urlOf endpoint
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


deleteRequest : Maybe String -> Endpoint -> Expect msg -> Cmd msg
deleteRequest token endpoint expect =
    Http.request
        { method = "DELETE"
        , headers = getHeaders token
        , url = urlOf endpoint
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


putRequest : Maybe String -> Endpoint -> Body -> Expect msg -> Cmd msg
putRequest token endpoint body expect =
    Http.request
        { method = "PUT"
        , headers = getHeaders token
        , url = urlOf endpoint
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
