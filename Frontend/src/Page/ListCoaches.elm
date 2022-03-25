module Page.ListCoaches exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Custom.Attributes
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.Coach exposing (Coach, CoachId, coachsDecoder)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import RemoteData exposing (WebData)
import Route exposing (Route(..), pushUrl)



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
    | EditCoachButtonClick CoachId
    | CoachDeleted (Result Http.Error DeleteResponse)



-- Init --


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( { coaches = RemoteData.Loading
      , navkey = navkey
      , deleteError = Nothing
      }
    , getCoachesRequest
    )



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

        EditCoachButtonClick id ->
            ( model, pushUrl (EditCoach id) model.navkey )

        DeleteCoachButtonClick id ->
            ( model, deleteCoachRequest id )

        CoachDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getCoachesRequest )

        CoachDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Coach not found."



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
        [ div Custom.Attributes.row [ viewRefreshButton ]
        , viewCoachesOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    div [ Custom.Attributes.col ]
        [ button
            [ onClick FetchCoaches
            , Custom.Attributes.refreshButton
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
    div [ Custom.Attributes.errorMessage ]
        [ h3 [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]


viewCoaches : List Coach -> Html Msg
viewCoaches coaches =
    div []
        [ viewHeader
        , table [ Custom.Attributes.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewCoach coaches
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ] [ h3 [] [ text "Coaches" ] ]
        , div [ Custom.Attributes.col ] [ viewToolBar ]
        ]


viewToolBar : Html Msg
viewToolBar =
    div [ Custom.Attributes.rightSideButtons ]
        [ button
            [ Custom.Attributes.addButton
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
        , td [ Custom.Attributes.tableButtonColumn ]
            [ viewEditButton coach, viewDeleteButton coach ]
        ]


viewDeleteButton : Coach -> Html Msg
viewDeleteButton coach =
    button
        (onClick (DeleteCoachButtonClick coach.id) :: Custom.Attributes.deleteButton)
        [ text "Delete" ]


viewEditButton : Coach -> Html Msg
viewEditButton coach =
    button
        (onClick (EditCoachButtonClick coach.id) :: Custom.Attributes.editButton)
        [ text "Edit" ]
