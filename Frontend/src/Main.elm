module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser
import Html.Events exposing (onInput)


type alias User =
    { name : String
    , email : String
    , password : String
    , loggedIn : Bool
    }

initial : User
initial = 
    { name = ""
    , email = ""
    , password = ""
    , loggedIn = False
    }


type Msg
    = SaveName String
    | SaveEmail String
    | SavePassword String
    | Signup


update : Msg -> User -> User
update message user =
    case message of
        SaveName newName ->
            { user | name = newName }
        
        SaveEmail newEmail ->
            { user | email = newEmail }

        SavePassword newPass ->
            { user | password = newPass }
        
        Signup ->
            { user | loggedIn = True }


view : User -> Html Msg
view user =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-md-6 col-md-offset-3" ]
                [ h1 [ class "text-center" ] [ text "Sign up" ]
                , Html.form []
                    [ div [ class "form-group" ]
                        [ label
                            [ class "control-label"
                            , for "name"
                            ]
                            [ text "Name" ]
                        , input
                            [ class "form-control"
                            , id "name"
                            , type_ "text"
                            , onInput SaveName
                            ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label
                            [ class "control-label"
                            , for "email"
                            ]
                            [ text "Email" ]
                        , input
                            [ class "form-control"
                            , id "email"
                            , type_ "email"
                            , onInput SaveEmail
                            ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label
                            [ class "control-label"
                            , for "password"
                            ]
                            [ text "Password" ]
                        , input
                            [ class "form-control"
                            , id "password"
                            , type_ "password"
                            , onInput SavePassword
                            ]
                            []
                        ]
                    , div [ class "text-center" ]
                        [ button
                            [ class "btn btn-lg btn-primary"
                            , type_ "submit"
                            , onClick Signup
                            ]
                            [ text "Create my account" ]
                        ]
                    ]
                ]
            ]
        ]


main : Program () User Msg
main =
    Browser.sandbox
        { init = initial
        , view = view
        , update = update
        }