module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Custom.Attributes
import Header
import Html exposing (..)
import Model.Session exposing (..)
import Page
import Route exposing (Route(..))
import Url exposing (Url)



-- Types --


type alias Model =
    { route : Route
    , headerModel : Header.Model
    , page : Page.Model
    , session : Session
    }


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | HeaderMsg Header.Msg
    | PageMsg Page.Msg



-- Init --


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navkey =
    let
        session =
            defaultSession navkey

        ( navModel, navCommand ) =
            Header.init session

        ( page, pageCommand ) =
            Page.init session <| Route.parseUrl url

        model =
            { route = Route.parseUrl url
            , headerModel = navModel
            , page = page
            , session = session
            }

        cmds =
            Cmd.batch
                [ Cmd.map HeaderMsg navCommand
                , Cmd.map PageMsg pageCommand
                ]
    in
    ( model, cmds )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.session.navkey <| Url.toString url
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            let
                newRoute =
                    Route.parseUrl url

                ( newPage, pageCmds ) =
                    Page.init model.session newRoute
            in
            ( { model | route = newRoute, page = newPage }
            , Cmd.map PageMsg pageCmds
            )

        HeaderMsg subMsg ->
            let
                ( newModel, newCmd ) =
                    Header.update subMsg model.headerModel
            in
            ( { model | headerModel = newModel }
            , Cmd.map HeaderMsg newCmd
            )

        PageMsg subMsg ->
            let
                ( newPage, pageCmds ) =
                    Page.update subMsg model.page
            in
            ( { model | page = newPage }
            , Cmd.map PageMsg pageCmds
            )



-- View --


view : Model -> Document Msg
view model =
    { title = "FTBBL"
    , body =
        [ div [ Custom.Attributes.mainContainer ]
            [ navView model
            , currentPageView model
            ]
        ]
    }


navView : Model -> Html Msg
navView model =
    Header.view model.headerModel
        |> Html.map HeaderMsg


currentPageView : Model -> Html Msg
currentPageView model =
    Page.view model.page
        |> Html.map PageMsg



-- Main --


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = unsubscribe
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


unsubscribe : Model -> Sub msg
unsubscribe _ =
    Sub.none
