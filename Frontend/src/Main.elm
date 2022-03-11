module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Page.ListTeams as ListTeams
import Page.ListCoaches as ListCoaches
import Header
import Route exposing (Route)
import Url exposing (Url)
import Html.Attributes exposing (class)
import Model.Coach exposing (Coach)


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


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        (navModel, navCommand) = 
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
                    let
                        ( pageModel, pageCmds ) =
                            ListTeams.init
                    in
                    ( TeamsPage pageModel, Cmd.map TeamsPageMsg pageCmds )

                Route.Coaches ->
                    let
                        ( pageModel, pageCmds ) =
                            ListCoaches.init
                    in
                    ( CoachesPage pageModel, Cmd.map CoachesPageMsg pageCmds )

    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


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

        ( HeaderMsg subMsg, _) ->
            let
                ( newModel, newCmd ) =
                    Header.update subMsg model.headerModel
            in
            ( { model | headerModel = newModel }
            , Cmd.map HeaderMsg newCmd
            )


        ( TeamsPageMsg subMsg, TeamsPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListTeams.update subMsg pageModel
            in
            ( { model | page = TeamsPage updatedPageModel }
            , Cmd.map TeamsPageMsg updatedCmd
            )

        ( CoachesPageMsg subMsg, CoachesPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListCoaches.update subMsg pageModel
            in
            ( { model | page = CoachesPage updatedPageModel }
            , Cmd.map CoachesPageMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


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
