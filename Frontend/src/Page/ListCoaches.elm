module Page.ListCoaches exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Error
import Fcss
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.Coach exposing (Coach, CoachId, coachsDecoder)
import Model.DeleteResponse exposing (DeleteResponse)
import RemoteData exposing (WebData)
import Route exposing (Route(..), pushUrl)
import Model.DeleteResponse exposing (deleteResponseDecoder)
import Error exposing (buildErrorMessage)



-- Types --


type alias Model =
    { coaches : WebData (List Coach)
    , navkey : Nav.Key
    , deleteError : Maybe String
    }


type Msg
    = FetchCoaches
    | CoachesRecieved (WebData (List Coach))
    | AddCoachButtonClick
    | DeleteCoachButtonClick CoachId
    | CoachDeleted (Result Http.Error DeleteResponse)



-- Init --


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( 
        { coaches = RemoteData.Loading
        , navkey = navkey
        , deleteError=Nothing 
        }
    , getCoachesRequest )



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

        DeleteCoachButtonClick id ->
            ( model, deleteCoachRequest id )

        CoachDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getCoachesRequest)

        CoachDeleted (Err err) ->
            ( {model | deleteError = Just (buildErrorMessage err)}, Cmd.none)


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then Nothing else Just "Delete Failed. Coach not found."

-- Common Helpers --


getCoachesRequest : Cmd Msg
getCoachesRequest =
    Api.getRequest Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder


deleteCoachRequest : CoachId -> Cmd Msg
deleteCoachRequest id =
    Api.deleteRequest (Api.Coach id) <|
        Http.expectJson CoachDeleted deleteResponseDecoder

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


viewDeleteButton : Coach -> Html Msg
viewDeleteButton coach =
    button
        ( onClick (DeleteCoachButtonClick coach.id) :: Fcss.deleteButton )
        [ text "Delete" ]


viewEditButton : Coach -> Html Msg
viewEditButton _ =
    button
        Fcss.editButton
        [ text "Edit" ]
