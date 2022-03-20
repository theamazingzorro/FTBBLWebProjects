module Header exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Fcss
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Route exposing (Route(..), pushUrl)



-- Types --


type alias Model =
    { navkey : Nav.Key
    }


type Msg
    = TeamIndexClicked
    | CoachIndexClicked



-- Init --


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( { navkey = navkey }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamIndexClicked ->
            ( model, pushUrl Route.Teams model.navkey )

        CoachIndexClicked ->
            ( model, pushUrl Route.Coaches model.navkey )



-- View --


view : Model -> Html Msg
view _ =
    nav [ Fcss.mainNavBar ]
        [ a [ Fcss.navBarBrand ] [ text "FTBBL" ]
        , toggleBarButton
        , div [ Fcss.navBarCollapsable, id "navbarNav" ]
            [ ul [ Fcss.navBarLinkList ]
                [ linkElement "Teams" TeamIndexClicked
                , linkElement "Coaches" CoachIndexClicked
                ]
            ]
        ]


toggleBarButton : Html Msg
toggleBarButton =
    button
        [ class "navbar-toggler"
        , type_ "button"
        , attribute "data-toggle" "collapse"
        , attribute "data-target" "#navbarNav"
        ]
        [ span [ class "navbar-toggler-icon" ] [] ]


linkElement : String -> Msg -> Html Msg
linkElement title msg =
    li [ Fcss.navItem ]
        [ a
            [ Fcss.navLink
            , onClick msg
            , href "#"
            ]
            [ text title ]
        ]
