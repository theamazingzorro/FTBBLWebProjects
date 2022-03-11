
module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
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
