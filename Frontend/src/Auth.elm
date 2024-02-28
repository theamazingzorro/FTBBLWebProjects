module Auth exposing (..)

import Html exposing (Html, text)
import Model.Session exposing (Session)


requiresAuth : Session -> Html msg -> Html msg
requiresAuth session value =
    case session.token of
        Just _ ->
            value

        Nothing ->
            text ""
