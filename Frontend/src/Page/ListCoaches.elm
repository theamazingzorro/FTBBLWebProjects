module Page.ListCoaches exposing (Model, Msg, init, update, view)

import Api
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.Coach exposing (Coach, coachsDecoder)
import RemoteData exposing (WebData)



-- Types --


type alias Model =
    { coaches : WebData (List Coach)
    }


type Msg
    = FetchCoaches
    | CoachesRecieved (WebData (List Coach))



-- Init --


init : ( Model, Cmd Msg )
init =
    ( { coaches = RemoteData.Loading }, getCoachesRequest )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchCoaches ->
            ( { model | coaches = RemoteData.Loading }, getCoachesRequest )

        CoachesRecieved response ->
            ( { model | coaches = response }, Cmd.none )



-- Common Helpers --


getCoachesRequest : Cmd Msg
getCoachesRequest =
    Api.getRequest Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ div [ class "row", style "padding-bottom" "6px" ] [ viewRefreshButton ]
        , viewCoachesOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    div [ class "col" ]
        [ button
            [ onClick FetchCoaches
            , class "btn btn-sm btn-secondary float-end"
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
    div []
        [ h3 [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]


viewCoaches : List Coach -> Html Msg
viewCoaches coaches =
    div []
        [ viewHeader
        , table [ class "table table-striped table-hover" ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewCoach coaches
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "row" ]
        [ div [ class "col" ] [ h3 [] [ text "Coaches" ] ]
        , div [ class "col" ] [ viewToolBar ]
        ]


viewToolBar : Html Msg
viewToolBar =
    div [ class "btn-group float-end" ]
        [ button [ class "btn btn-success" ] [ text "Add Coach" ]
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
        , td [ width 175 ]
            [ viewEditButton coach, viewDeleteButton coach ]
        ]


viewDeleteButton : Coach -> Html msg
viewDeleteButton _ =
    button
        [ class "btn btn-danger"
        , style "margin-left" "6px"
        ]
        [ text "Delete" ]


viewEditButton : Coach -> Html msg
viewEditButton _ =
    button
        [ class "btn btn-secondary"
        , style "margin-left" "6px"
        ]
        [ text "Edit" ]
