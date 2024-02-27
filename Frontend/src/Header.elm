module Header exposing (Model, Msg, OutMsg(..), init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Html exposing (hidden, importantNavButton, visible)
import Env exposing (leagueName)
import Html exposing (Attribute, Html, text)
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
    , shouldDisplaySidebar : Bool
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
    | ToggleSidebar
    | HideSidebar


type OutMsg
    = Signout



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , divisions = RemoteData.Loading
      , shouldDisplaySidebar = False
      }
    , getDivisionsRequest session.token
    )



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

        ToggleSidebar ->
            ( { model | shouldDisplaySidebar = not model.shouldDisplaySidebar }, Cmd.none, Nothing )

        HideSidebar ->
            ( { model | shouldDisplaySidebar = False }, Cmd.none, Nothing )


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


view : Model -> List (Html Msg)
view model =
    [ viewNavbar
    , viewSidebar model
    , Custom.Html.sideBarShadow [ onClick HideSidebar, displayStyle model.shouldDisplaySidebar ]
    ]


viewNavbar : Html Msg
viewNavbar =
    Custom.Html.navBar []
        [ toggleBarButton
        , importantNavButton [ onClick HomeClicked ] [ text leagueName ]
        , navLink "Teams" TeamIndexClicked
        , navLink "Coaches" CoachIndexClicked
        , navLink "Divisions" DivisionIndexClicked
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    Custom.Html.sideBar [ displayStyle model.shouldDisplaySidebar ]
        [ Custom.Html.closeSideBarButton []
        , Custom.Html.sideBarTitle [] [ text "Menu" ]
        , Custom.Html.list []
            [ sidebarLink "Teams" TeamIndexClicked
            , sidebarLink "Coaches" CoachIndexClicked
            , sidebarLink "Divisions" DivisionIndexClicked
            , viewDivisionsLinks model.divisions
            , sidebarLink "Head to Head" HeadToHeadClicked
            , requiresAuth model.session <| sidebarLink "Accolades" AccoladeIndexClicked
            , viewSignInOutLink model.session.token
            ]
        ]


displayStyle : Bool -> Attribute msg
displayStyle shouldDisplay =
    if shouldDisplay then
        visible

    else
        hidden


viewSignInOutLink : Maybe String -> Html Msg
viewSignInOutLink token =
    case token of
        Just _ ->
            sidebarLink "Sign Out" SignoutClicked

        Nothing ->
            sidebarLink "Sign In" SigninClicked


viewDivisionsLinks : WebData (List Division) -> Html Msg
viewDivisionsLinks divisions =
    case divisions of
        RemoteData.Success divs ->
            Custom.Html.list []
                (List.map
                    (\div -> smallSidebarLink (div.name ++ " Season " ++ String.fromInt div.season) <| SpecificDivisionClicked div.id)
                    (List.sortWith compareDivisions divs
                        |> List.filter (\div -> not div.closed)
                    )
                )

        _ ->
            text ""


toggleBarButton : Html Msg
toggleBarButton =
    Custom.Html.menuIcon [ onClick ToggleSidebar ]


navLink : String -> Msg -> Html Msg
navLink title msg =
    Custom.Html.navButton [ onClick msg ] [ text title ]


sidebarLink : String -> Msg -> Html Msg
sidebarLink title msg =
    Custom.Html.sideBarLink [ onClick msg ] [ text title ]


smallSidebarLink : String -> Msg -> Html Msg
smallSidebarLink title msg =
    Custom.Html.smallSideBarLink [ onClick msg ] [ text title ]
