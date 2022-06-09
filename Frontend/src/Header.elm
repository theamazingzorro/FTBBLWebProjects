module Header exposing (Model, Msg, init, update, view)

import Custom.Attributes
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model.Session exposing (Session)
import Route exposing (Route(..), pushUrl)



-- Types --


type alias Model =
    { session : Session
    }


type Msg
    = HomeClicked
    | SigninClicked
    | TeamIndexClicked
    | CoachIndexClicked
    | DivisionIndexClicked



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamIndexClicked ->
            ( model, pushUrl model.session.navkey Route.Teams )

        CoachIndexClicked ->
            ( model, pushUrl model.session.navkey Route.Coaches )

        DivisionIndexClicked ->
            ( model, pushUrl model.session.navkey Route.Divisions )

        HomeClicked ->
            ( model, pushUrl model.session.navkey Route.Home )

        SigninClicked ->
            ( model, pushUrl model.session.navkey Route.Signin )



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
                , linkElement "Divisions" DivisionIndexClicked
                , linkElement "Sign In" SigninClicked
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
