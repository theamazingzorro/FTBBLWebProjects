module Header exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Custom.Attributes
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
    | HomeClicked



-- Init --


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( { navkey = navkey }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamIndexClicked ->
            ( model, pushUrl model.navkey Route.Teams )

        CoachIndexClicked ->
            ( model, pushUrl model.navkey Route.Coaches )

        HomeClicked ->
            ( model, pushUrl model.navkey Route.Home )



-- View --


view : Model -> Html Msg
view _ =
    nav [ Custom.Attributes.mainNavBar ]
        [ a
            [ Custom.Attributes.navBarBrand
            , onClick HomeClicked
            ]
            [ text "FTBBL" ]
        , toggleBarButton
        , div [ Custom.Attributes.navBarCollapsable, id "navbarNav" ]
            [ ul [ Custom.Attributes.navBarLinkList ]
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
    li [ Custom.Attributes.navItem ]
        [ a
            [ Custom.Attributes.navLink
            , onClick msg
            , href "#"
            ]
            [ text title ]
        ]
