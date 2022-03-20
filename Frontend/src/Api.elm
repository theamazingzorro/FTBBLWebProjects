module Api exposing (Endpoint(..), getRequest)

import Http exposing (Expect)


type Endpoint
    = Team
    | Coach


baseUrl : String
baseUrl =
    "https://localhost:17317/api/"


stringOf : Endpoint -> String
stringOf endpoint =
    case endpoint of
        Team ->
            "team"

        Coach ->
            "coach"


getRequest : Endpoint -> Expect msg -> Cmd msg
getRequest endpoint expect =
    Http.get
        { url = baseUrl ++ stringOf endpoint
        , expect = expect
        }
