module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Header
import Html exposing (..)
import Html.Attributes exposing (class)
import Page.ListCoaches as ListCoaches
import Page.ListTeams as ListTeams
import Route exposing (Route(..))
import Url exposing (Url)



-- Types --


type alias Model =
    { route : Route
    , headerModel : Header.Model
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | TeamsPage ListTeams.Model
    | CoachesPage ListCoaches.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | HeaderMsg Header.Msg
    | TeamsPageMsg ListTeams.Msg
    | CoachesPageMsg ListCoaches.Msg



-- Init --


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        ( navModel, navCommand ) =
            Header.init

        model =
            { route = Route.parseUrl url
            , headerModel = navModel
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.map HeaderMsg navCommand )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Teams ->
                    initPage ListTeams.init TeamsPage TeamsPageMsg

                Route.Coaches ->
                    initPage ListCoaches.init CoachesPage CoachesPageMsg
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


initPage : ( subModel, Cmd subMsg ) -> (subModel -> Page) -> (subMsg -> Msg) -> ( Page, Cmd Msg )
initPage initFunc pageWrap msgWrap =
    let
        ( newModel, newCmds ) =
            initFunc
    in
    ( pageWrap newModel, Cmd.map msgWrap newCmds )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey <| Url.toString url
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( HeaderMsg subMsg, _ ) ->
            let
                ( newModel, newCmd ) =
                    Header.update subMsg model.headerModel
            in
            ( { model | headerModel = newModel }
            , Cmd.map HeaderMsg newCmd
            )

        ( TeamsPageMsg subMsg, TeamsPage pageModel ) ->
            ListTeams.update subMsg pageModel
                |> updateWith TeamsPage TeamsPageMsg model

        ( CoachesPageMsg subMsg, CoachesPage pageModel ) ->
            ListCoaches.update subMsg pageModel
                |> updateWith CoachesPage CoachesPageMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- View --


view : Model -> Document Msg
view model =
    { title = "FTBBL"
    , body =
        [ div [ class "container" ]
            [ navView model
            , currentView model
            ]
        ]
    }


navView : Model -> Html Msg
navView model =
    case model.page of
        NotFoundPage ->
            div [] []

        _ ->
            Header.view model.headerModel
                |> Html.map HeaderMsg


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        TeamsPage pageModel ->
            ListTeams.view pageModel
                |> Html.map TeamsPageMsg

        CoachesPage pageModel ->
            ListCoaches.view pageModel
                |> Html.map CoachesPageMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]



-- Main --


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
