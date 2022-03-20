module Page.ListCoaches exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Error
import Fcss
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.Coach exposing (Coach, coachsDecoder)
import RemoteData exposing (WebData)
import Route exposing (Route(..), pushUrl)



-- Types --


type alias Model =
    { coaches : WebData (List Coach)
    , navkey : Nav.Key
    }


type Msg
    = FetchCoaches
    | CoachesRecieved (WebData (List Coach))
    | AddCoachButtonClick



-- Init --


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( { coaches = RemoteData.Loading, navkey = navkey }, getCoachesRequest )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchCoaches ->
            ( { model | coaches = RemoteData.Loading }, getCoachesRequest )

        CoachesRecieved response ->
            ( { model | coaches = response }, Cmd.none )

        AddCoachButtonClick ->
            ( model, pushUrl AddCoach model.navkey )



-- Common Helpers --


getCoachesRequest : Cmd Msg
getCoachesRequest =
    Api.getRequest Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ div Fcss.row [ viewRefreshButton ]
        , viewCoachesOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    div [ Fcss.col ]
        [ button
            [ onClick FetchCoaches
            , Fcss.refreshButton
            ]
            [ text "Refresh Coaches" ]
        ]


viewCoachesOrError : Model -> Html Msg
viewCoachesOrError model =
    case model.coaches of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success coaches ->
            viewCoaches coaches

        RemoteData.Failure httpError ->
            viewError <| Error.buildErrorMessage httpError


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div [ Fcss.errorMessage ]
        [ h3 [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]


viewCoaches : List Coach -> Html Msg
viewCoaches coaches =
    div []
        [ viewHeader
        , table [ Fcss.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewCoach coaches
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div Fcss.row
        [ div [ Fcss.col ] [ h3 [] [ text "Coaches" ] ]
        , div [ Fcss.col ] [ viewToolBar ]
        ]


viewToolBar : Html Msg
viewToolBar =
    div [ Fcss.rightSideButtons ]
        [ button
            [ Fcss.addButton
            , onClick AddCoachButtonClick
            ]
            [ text "Add Coach" ]
        ]


viewTableHeader : Html Msg
viewTableHeader =
    thead []
        [ tr []
            [ th [ scope "col" ]
                [ text "Name" ]
            , th [ scope "col" ]
                [ text "Elo" ]
            , th [ scope "col" ]
                [ text "" ]
            ]
        ]


viewCoach : Coach -> Html Msg
viewCoach coach =
    tr []
        [ td []
            [ text coach.name ]
        , td []
            [ text <| String.fromInt coach.elo ]
        , td [ Fcss.tableButtonColumn ]
            [ viewEditButton coach, viewDeleteButton coach ]
        ]


viewDeleteButton : Coach -> Html msg
viewDeleteButton _ =
    button
        Fcss.deleteButton
        [ text "Delete" ]


viewEditButton : Coach -> Html msg
viewEditButton _ =
    button
        Fcss.editButton
        [ text "Edit" ]
