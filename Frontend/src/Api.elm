module Api exposing (getRequest)

import Http exposing (Expect)

baseUrl : String
baseUrl = "https://localhost:17317/api/"

getRequest : String -> Expect msg -> Cmd msg
getRequest endpoint expect =
    Http.get
        { url = baseUrl ++ endpoint
        , expect = expect
        }