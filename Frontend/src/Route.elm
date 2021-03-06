module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Model.Coach as Coach exposing (CoachId)
import Model.Division as Div exposing (DivisionId)
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
    | Divisions
    | AddDivision
    | EditDivision DivisionId


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
                [ s "Team" </> s "Add"
                , s "team" </> s "add"
                ]
        , map EditTeam <|
            oneOf
                [ s "Team" </> s "Edit" </> Team.idParser
                , s "team" </> s "edit" </> Team.idParser
                ]

        {- Division CRUD -}
        , map Divisions <| oneOf [ s "Division", s "division" ]
        , map AddDivision <|
            oneOf
                [ s "Division" </> s "Add"
                , s "division" </> s "add"
                ]
        , map EditDivision <|
            oneOf
                [ s "Division" </> s "Edit" </> Div.idParser
                , s "division" </> s "edit" </> Div.idParser
                ]

        {- Coach CRUD -}
        , map Coaches <| oneOf [ s "Coach", s "coach" ]
        , map AddCoach <|
            oneOf
                [ s "Coach" </> s "Add"
                , s "coach" </> s "add"
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

        Divisions ->
            "/Division"

        AddDivision ->
            "/Division/Add"

        EditDivision divisionId ->
            "/Division/Edit/" ++ Div.idToString divisionId
