module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = NotFound
    | Coaches
    | AddCoach
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
        , map Teams <| oneOf [ s "Teams", s "teams" ]
        , map Coaches <| oneOf [ s "Coaches", s "coaches" ]
        , map AddCoach <| oneOf [ s "AddCoaches", s "addcoaches" ]
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
            "/Teams"

        Coaches ->
            "/Coaches"

        AddCoach ->
            "/AddCoaches"
