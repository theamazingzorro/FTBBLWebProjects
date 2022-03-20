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
    Api.getRequest Api.Coach <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick FetchCoaches ]
            [ text "Refresh Coaches" ]
        , viewCoachesOrError model
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
        [ h3 [] [ text "Coaches" ]
        , table [ class "table table-striped table-hover" ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewCoach coaches
            ]
        ]


viewTableHeader : Html Msg
viewTableHeader =
    thead []
        [ tr []
            [ th [ scope "col" ]
                [ text "Name" ]
            , th [ scope "col" ]
                [ text "Elo" ]
            ]
        ]


viewCoach : Coach -> Html Msg
viewCoach coach =
    tr []
        [ td []
            [ text coach.name ]
        , td []
            [ text <| String.fromInt coach.elo ]
        ]
