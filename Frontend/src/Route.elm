module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Model.Coach as Coach exposing (CoachId)
import Model.Team as Team exposing (TeamId)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Home
    | Coaches
    | AddCoach
    | EditCoach CoachId
    | Teams
    | AddTeam
    | EditTeam TeamId


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Home top

        {- Team CRUD -}
        , map Teams <| oneOf [ s "Team", s "team" ]
        , map AddTeam <|
            oneOf
                [ s "team" </> s "add"
                , s "Team" </> s "Add"
                ]
        , map EditTeam <|
            oneOf
                [ s "Team" </> s "Edit" </> Team.idParser
                , s "team" </> s "edit" </> Team.idParser
                ]

        {- Coach CRUD -}
        , map Coaches <| oneOf [ s "Coach", s "coach" ]
        , map AddCoach <|
            oneOf
                [ s "coach" </> s "add"
                , s "Coach" </> s "Add"
                ]
        , map EditCoach <|
            oneOf
                [ s "Coach" </> s "Edit" </> Coach.idParser
                , s "coach" </> s "edit" </> Coach.idParser
                ]
        ]


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl navkey route =
    routeToString route
        |> Nav.pushUrl navkey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/NotFound"

        Home ->
            "/"

        Teams ->
            "/Team"

        AddTeam ->
            "/Team/Add"

        EditTeam id ->
            "/Team/Edit/" ++ Team.idToString id

        Coaches ->
            "/Coach"

        AddCoach ->
            "/Coach/Add"

        EditCoach coachId ->
            "/Coach/Edit/" ++ Coach.idToString coachId
