module Header exposing (Model, Msg, OutMsg(..), init, update, view)

import Api
import Custom.Attributes
import Env exposing (leagueName)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.Division exposing (Division, DivisionId, compareDivisions, divisionsDecoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (Route(..), pushUrl)



-- Types --


type alias Model =
    { session : Session
    , divisions : WebData (List Division)
    }


type Msg
    = HomeClicked
    | SigninClicked
    | SignoutClicked
    | TeamIndexClicked
    | CoachIndexClicked
    | DivisionIndexClicked
    | AccoladeIndexClicked
    | HeadToHeadClicked
    | SpecificDivisionClicked DivisionId
    | DivisionsRecieved (WebData (List Division))


type OutMsg
    = Signout



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, divisions = RemoteData.Loading }, getDivisionsRequest session.token )



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

        AccoladeIndexClicked ->
            ( model, pushUrl model.session.navkey Route.Accolades, Nothing )

        SpecificDivisionClicked divId ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision divId, Nothing )

        HomeClicked ->
            ( model, pushUrl model.session.navkey Route.Home, Nothing )

        HeadToHeadClicked ->
            ( model, pushUrl model.session.navkey Route.ViewHeadToHeadDefault, Nothing )

        SigninClicked ->
            ( model, pushUrl model.session.navkey Route.Signin, Nothing )

        SignoutClicked ->
            let
                updateSession session =
                    { session | token = Nothing }
            in
            ( { model | session = updateSession model.session }, Cmd.none, Just Signout )

        DivisionsRecieved (RemoteData.Success divs) ->
            ( { model | divisions = RemoteData.Success <| cleanupDivList divs }, Cmd.none, Nothing )

        DivisionsRecieved response ->
            ( { model | divisions = response }, Cmd.none, Nothing )


cleanupDivList : List Division -> List Division
cleanupDivList divs =
    List.sortBy .season divs
        |> List.reverse
        |> List.take 10



-- API Requests --


getDivisionsRequest : Maybe String -> Cmd Msg
getDivisionsRequest token =
    Api.getRequest token Api.Divisions <|
        Http.expectJson (RemoteData.fromResult >> DivisionsRecieved) divisionsDecoder



-- View --


view : Model -> Html Msg
view model =
    nav [ Custom.Attributes.mainNavBar ]
        [ styleTag
        , a
            [ Custom.Attributes.navBarBrand
            , onClick HomeClicked
            ]
            [ text leagueName ]
        , toggleBarButton
        , div [ Custom.Attributes.navBarCollapsable, id "navbarNav" ]
            [ ul [ Custom.Attributes.navBarLinkList ]
                [ linkElement "Teams" TeamIndexClicked
                , linkElement "Coaches" CoachIndexClicked
                , viewDivisionsLink model.divisions
                , linkElement "Matchups" HeadToHeadClicked
                , viewAccoladesLink model.session.token
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


viewAccoladesLink : Maybe String -> Html Msg
viewAccoladesLink token =
    case token of
        Just _ ->
            linkElement "Accolades" AccoladeIndexClicked

        Nothing ->
            text ""


viewDivisionsLink : WebData (List Division) -> Html Msg
viewDivisionsLink divisions =
    case divisions of
        RemoteData.Success divs ->
            dropdownLink "Divisions" DivisionIndexClicked <|
                List.map
                    (\div -> dropdownEntry (div.name ++ " Season " ++ String.fromInt div.season) <| SpecificDivisionClicked div.id)
                    (List.sortWith compareDivisions divs
                        |> List.filter (\div -> not div.closed)
                    )

        _ ->
            linkElement "Divisions" DivisionIndexClicked


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


dropdownLink : String -> Msg -> List (Html Msg) -> Html Msg
dropdownLink title clickEvent submenu =
    li [ Custom.Attributes.navDropDownContainer ]
        [ a
            (onClick clickEvent
                :: Custom.Attributes.navDropDownTitleLink
            )
            [ text title ]
        , ul [ Custom.Attributes.navDropDownMenu ]
            submenu
        ]


dropdownEntry : String -> Msg -> Html Msg
dropdownEntry label clickEvent =
    li
        [ Custom.Attributes.navDropDownItem
        , onClick clickEvent
        ]
        [ text label ]



{- Direct css to make the hover function in both dev and live. -}


styleTag : Html Msg
styleTag =
    let
        styles =
            """
        @media all and (min-width: 992px) {
            .navbar .nav-item .dropdown-menu{ display: none; }
            .navbar .nav-item:hover .nav-link{   }
            .navbar .nav-item:hover .dropdown-menu{ display: block; }
            .navbar .nav-item .dropdown-menu{ margin-top:0; }
        }
      """
    in
    node "style" [] [ text styles ]
