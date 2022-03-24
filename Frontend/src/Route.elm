module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Model.Coach as Coach exposing (CoachId)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Coaches
    | AddCoach
    | EditCoach CoachId
    | Teams


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
        [ map Teams top
        , map Teams <| oneOf [ s "Team", s "team" ]
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


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/NotFound"

        Teams ->
            "/Team"

        Coaches ->
            "/Coach"

        AddCoach ->
            "/Coach/Add"

        EditCoach coachId ->
            "/Coach/Edit/" ++ Coach.idToString coachId
