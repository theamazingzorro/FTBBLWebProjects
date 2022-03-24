module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Fcss
import Header
import Html exposing (..)
import Page.AddCoach as AddCoach
import Page.EditCoach as EditCoach
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
    | AddCoachPage AddCoach.Model
    | EditCoachPage EditCoach.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | HeaderMsg Header.Msg
    | TeamsPageMsg ListTeams.Msg
    | CoachesPageMsg ListCoaches.Msg
    | AddCoachPageMsg AddCoach.Msg
    | EditCoachPageMsg EditCoach.Msg



-- Init --


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        ( navModel, navCommand ) =
            Header.init navKey

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
                    initPage (ListCoaches.init model.navKey) CoachesPage CoachesPageMsg

                Route.AddCoach ->
                    initPage AddCoach.init AddCoachPage AddCoachPageMsg

                Route.EditCoach id ->
                    initPage (EditCoach.init id) EditCoachPage EditCoachPageMsg
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

        ( TeamsPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( CoachesPageMsg subMsg, CoachesPage pageModel ) ->
            ListCoaches.update subMsg pageModel
                |> updateWith CoachesPage CoachesPageMsg model

        ( CoachesPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( AddCoachPageMsg subMsg, AddCoachPage pageModel ) ->
            AddCoach.update subMsg pageModel
                |> updateWith AddCoachPage AddCoachPageMsg model

        ( AddCoachPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditCoachPageMsg subMsg, EditCoachPage pageModel ) ->
            EditCoach.update subMsg pageModel
                |> updateWith EditCoachPage EditCoachPageMsg model

        ( EditCoachPageMsg _, _ ) ->
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
        [ div [ Fcss.mainContainer ]
            [ navView model
            , currentView model
            ]
        ]
    }


navView : Model -> Html Msg
navView model =
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

        AddCoachPage pageModel ->
            AddCoach.view pageModel
                |> Html.map AddCoachPageMsg

        EditCoachPage pageModel ->
            EditCoach.view pageModel
                |> Html.map EditCoachPageMsg


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
