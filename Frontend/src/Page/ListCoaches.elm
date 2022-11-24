module Page.ListCoaches exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Attributes
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.Coach exposing (Coach, CoachId, coachsDecoder)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { coaches : WebData (List Coach)
    , session : Session
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


init : Session -> ( Model, Cmd Msg )
init session =
    ( { coaches = RemoteData.Loading
      , session = session
      , deleteError = Nothing
      }
    , getCoachesRequest session.token
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchCoaches ->
            ( { model | coaches = RemoteData.Loading }, getCoachesRequest model.session.token )

        CoachesRecieved response ->
            ( { model | coaches = response }, Cmd.none )

        AddCoachButtonClick ->
            ( model, pushUrl model.session.navkey Route.AddCoach )

        EditCoachButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditCoach id )

        DeleteCoachButtonClick id ->
            ( model, deleteCoachRequest model.session.token id )

        CoachDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getCoachesRequest model.session.token  )

        CoachDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Coaches cannot be deleted before their last team."



-- API Requests --


getCoachesRequest : Maybe String -> Cmd Msg
getCoachesRequest token =
    Api.getRequest token Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder


deleteCoachRequest : Maybe String -> CoachId -> Cmd Msg
deleteCoachRequest token id =
    Api.deleteRequest token (Api.Coach id) <|
        Http.expectJson CoachDeleted deleteResponseDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ div Custom.Attributes.row [ viewRefreshButton ]
        , viewErrorMessage model.deleteError
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
            viewCoaches model.session coaches

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div [ Custom.Attributes.errorMessage ]
        [ h3 [] [ text errorHeading ]
        , text <| "Error: " ++ errorMessage
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just m ->
            div [ Custom.Attributes.errorMessage ]
                [ text <| "Error: " ++ m ]

        Nothing ->
            text ""


viewCoaches : Session -> List Coach -> Html Msg
viewCoaches session coaches =
    div []
        [ viewHeader session
        , table [ Custom.Attributes.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map (viewCoach session) coaches
            ]
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ] [ h3 [] [ text "Coaches" ] ]
        , div [ Custom.Attributes.col ] [ requiresAuth session viewToolBar ]
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


viewCoach : Session -> Coach -> Html Msg
viewCoach session coach =
    tr []
        [ td []
            [ text coach.name ]
        , td []
            [ text <| String.fromInt coach.elo ]
        , requiresAuth session <|
            td [ Custom.Attributes.tableButtonColumn 2 ]
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
