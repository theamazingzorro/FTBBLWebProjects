module Main exposing (Msg, main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Custom.Html
import Env exposing (leagueName)
import Header
import Html exposing (Html, a, h4, p, text)
import Html.Attributes exposing (href, tabindex, target)
import Model.Session exposing (..)
import Page
import Page.Signin as SigninPage
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

                ( headModel, headCommand ) =
                    Header.init model.session
            in
            ( { model | route = newRoute, page = newPage, headerModel = headModel }
            , Cmd.batch [ Cmd.map PageMsg pageCmds, Cmd.map HeaderMsg headCommand ]
            )

        HeaderMsg subMsg ->
            let
                ( newHeader, headerCmds, headerOutMsg ) =
                    Header.update subMsg model.headerModel

                ( newModel, newCmds ) =
                    processHeaderOutMsg model headerOutMsg
            in
            ( { newModel | headerModel = newHeader }
            , Cmd.batch [ Cmd.map HeaderMsg headerCmds, newCmds ]
            )

        PageMsg subMsg ->
            let
                ( newPage, pageCmds, pageOutMsg ) =
                    Page.update subMsg model.page

                ( newModel, newCmds ) =
                    processPageOutMsg model pageOutMsg
            in
            ( { newModel | page = newPage }
            , Cmd.batch [ Cmd.map PageMsg pageCmds, newCmds ]
            )


processHeaderOutMsg : Model -> Maybe Header.OutMsg -> ( Model, Cmd Msg )
processHeaderOutMsg model outMsg =
    case outMsg of
        Just Header.Signout ->
            let
                updateSession session =
                    { session | token = Nothing }

                updateHeader header =
                    { header | session = updateSession header.session }
            in
            ( { model
                | session = updateSession model.session
                , headerModel = updateHeader model.headerModel
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


processPageOutMsg : Model -> Maybe Page.OutMsg -> ( Model, Cmd Msg )
processPageOutMsg model outMsg =
    case outMsg of
        Just (Page.SigninPageOutMsg (SigninPage.ChangeToken token)) ->
            let
                updateSession session =
                    { session | token = Just token }

                updateHeader header =
                    { header | session = updateSession header.session }
            in
            ( { model
                | session = updateSession model.session
                , headerModel = updateHeader model.headerModel
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )



-- View --


view : Model -> Document Msg
view model =
    { title = leagueName
    , body =
        navView model
            ++ [ Custom.Html.mainContainer []
                    [ Custom.Html.pageContent [] [ currentPageView model ]
                    , viewFooter
                    ]
               ]
    }


viewFooter : Html msg
viewFooter =
    Custom.Html.footer []
        [ h4 [] [ text "©2024 FTBBL" ] ]
        [ p []
            [ text "Powered by "
            , a [ href "https://www.w3schools.com/w3css/default.asp", target "_blank", tabindex -1 ] [ text "w3.css" ]
            ]
        ]


navView : Model -> List (Html Msg)
navView model =
    Header.view model.headerModel
        |> List.map (Html.map HeaderMsg)


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
