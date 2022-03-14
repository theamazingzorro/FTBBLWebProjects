module Header exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Route exposing (Route(..))
import Route exposing (pushUrl)
import Browser.Navigation as Nav
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)


-- Types --


type alias Model =
    { navkey: Nav.Key
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
            ( model, pushUrl Route.Teams model.navkey  )
        
        CoachIndexClicked ->
            ( model, pushUrl Route.Coaches model.navkey  )



-- View --


view : Model -> Html Msg
view _ =
    nav [ class "navbar navbar-expand-lg navbar-light bg-light"]
        [ a [ class "navbar-brand" ] [ text "FTBBL" ]
        , toggleBarButton
        , div [ class "collapse navbar-collapse", id "navbarNav"]
            [ ul [ class "navbar-nav" ] 
                [ linkElement "Teams" TeamIndexClicked
                , linkElement "Coaches" CoachIndexClicked]
            ]
        ]


toggleBarButton : Html Msg
toggleBarButton =
    button [ class "navbar-toggler"
           , type_ "button"
           , attribute "data-toggle" "collapse"
           , attribute "data-target" "#navbarNav"
           ] 
           [ span [ class "nav-bar-toggler-icon" ] [] ]


linkElement : String -> Msg -> Html Msg
linkElement title msg =
    li [ class "nav-item" ] 
        [ a [ class "nav-link", onClick msg, href "#" ] 
            [ text title ] 
        ]