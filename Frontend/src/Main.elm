module Main exposing (main)

import Browser
import Page.ListTeams as ListTeams
import Route exposing (Route)
import Url exposing (Url)
import Browser.Navigation as Nav
import Html exposing (..)
import Browser exposing (UrlRequest)
import Browser exposing (Document)

type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }

type Page   
    = NotFoundPage
    | ListPage ListTeams.Model


type Msg 
    = LinkClicked UrlRequest
    | UrlChanged Url
    | ListPageMsg ListTeams.Msg


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


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
                    ( ListPage pageModel, Cmd.map ListPageMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "FTBBL"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        ListPage pageModel ->
            ListTeams.view pageModel
                |> Html.map ListPageMsg


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

        ( ListPageMsg subMsg, ListPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListTeams.update subMsg pageModel
            in
            ( { model | page = ListPage updatedPageModel }
            , Cmd.map ListPageMsg updatedCmd
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
