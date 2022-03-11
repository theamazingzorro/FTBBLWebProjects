
module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
import Browser.Navigation as Nav
import Url.Parser exposing (..)


type Route
    = NotFound
    | Coaches
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
        , map Teams <| s "teams"
        , map Coaches <| s "coaches"
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Teams ->
            "/teams"

        Coaches ->
            "/coaches"