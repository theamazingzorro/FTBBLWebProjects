module Header exposing (Model, Msg, OutMsg(..), init, update, view)

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
    | SignoutClicked
    | TeamIndexClicked
    | CoachIndexClicked
    | DivisionIndexClicked


type OutMsg
    = Signout



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case msg of
        TeamIndexClicked ->
            ( model, pushUrl model.session.navkey Route.Teams, Nothing )

        CoachIndexClicked ->
            ( model, pushUrl model.session.navkey Route.Coaches, Nothing )

        DivisionIndexClicked ->
            ( model, pushUrl model.session.navkey Route.Divisions, Nothing )

        HomeClicked ->
            ( model, pushUrl model.session.navkey Route.Home, Nothing )

        SigninClicked ->
            ( model, pushUrl model.session.navkey Route.Signin, Nothing )

        SignoutClicked ->
            let
                updateSession session =
                    { session | token = Nothing }
            in
            ( { model | session = updateSession model.session }, Cmd.none, Just Signout )



-- View --


view : Model -> Html Msg
view model =
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
                , viewSignInOutLink model.session.token
                ]
            ]
        ]


viewSignInOutLink : Maybe String -> Html Msg
viewSignInOutLink token =
    case token of
        Just _ ->
            linkElement "Sign Out" SignoutClicked

        Nothing ->
            linkElement "Sign In" SigninClicked


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
