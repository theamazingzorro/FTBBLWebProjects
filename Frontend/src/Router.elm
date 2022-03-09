module Router exposing (..)

import Url.Parser exposing ((</>), Parser, int, map, oneOf, s)


type Route
    = TeamIndex
    | Team Int


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map TeamIndex (s "team")
        , map Team (s "team" </> int)
        ]
