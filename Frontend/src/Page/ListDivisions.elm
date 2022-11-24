module Page.ListDivisions exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Attributes
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Division exposing (Division, DivisionId, divisionsDecoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { divisions : WebData (List Division)
    , session : Session
    , deleteError : Maybe String
    }


type Msg
    = FetchDivisions
    | DivisionsRecieved (WebData (List Division))
    | AddDivisionButtonClick
    | EditDivisionButtonClick DivisionId
    | DeleteDivisionButtonClick DivisionId
    | DivisionDeleted (Result Http.Error DeleteResponse)
    | ViewDivisionButtonClick DivisionId



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { divisions = RemoteData.Loading
      , session = session
      , deleteError = Nothing
      }
    , getDivisionsRequest session.token
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDivisions ->
            ( { model | divisions = RemoteData.Loading }, getDivisionsRequest model.session.token)

        DivisionsRecieved response ->
            ( { model | divisions = response }, Cmd.none )

        AddDivisionButtonClick ->
            ( model, pushUrl model.session.navkey Route.AddDivision )

        EditDivisionButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditDivision id )

        ViewDivisionButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision id )

        DeleteDivisionButtonClick id ->
            ( model, deleteDivisionRequest model.session.token id )

        DivisionDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getDivisionsRequest model.session.token)

        DivisionDeleted (Err err) ->
            ( { model | deleteError = Just (Error.buildErrorMessage err) }, Cmd.none )


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. If this div ever had teams assigned it cannot be deleted."



-- API Requests --


getDivisionsRequest : Maybe String -> Cmd Msg
getDivisionsRequest token=
    Api.getRequest token Api.Divisions <|
        Http.expectJson (RemoteData.fromResult >> DivisionsRecieved) divisionsDecoder


deleteDivisionRequest : Maybe String -> DivisionId -> Cmd Msg
deleteDivisionRequest token id =
    Api.deleteRequest token (Api.Division id) <|
        Http.expectJson DivisionDeleted deleteResponseDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ div Custom.Attributes.row [ viewRefreshButton ]
        , viewErrorMessage model.deleteError
        , viewDivisionsOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    div [ Custom.Attributes.col ]
        [ button
            [ onClick FetchDivisions
            , Custom.Attributes.refreshButton
            ]
            [ text "Refresh Divisions" ]
        ]


viewDivisionsOrError : Model -> Html Msg
viewDivisionsOrError model =
    case model.divisions of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success divisions ->
            viewDivisions model.session divisions

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


viewDivisions : Session -> List Division -> Html Msg
viewDivisions session divisions =
    div []
        [ viewHeader session
        , table [ Custom.Attributes.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map (viewDivision session) divisions
            ]
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ] [ h3 [] [ text "Divisions" ] ]
        , div [ Custom.Attributes.col ] [ requiresAuth session viewToolBar ]
        ]


viewToolBar : Html Msg
viewToolBar =
    div [ Custom.Attributes.rightSideButtons ]
        [ button
            [ Custom.Attributes.addButton
            , onClick AddDivisionButtonClick
            ]
            [ text "Add Division" ]
        ]


viewTableHeader : Html Msg
viewTableHeader =
    thead []
        [ tr []
            [ th [ scope "col" ]
                [ text "Name" ]
            , th [ scope "col" ]
                [ text "Season" ]
            , th [ scope "col" ]
                [ text "" ]
            ]
        ]


viewDivision : Session -> Division -> Html Msg
viewDivision session division =
    tr []
        [ td [ class "btn-link", onClick <| ViewDivisionButtonClick division.id ]
            [ text division.name ]
        , td []
            [ text <| String.fromInt division.season ]
        , requiresAuth session <|
            td [ Custom.Attributes.tableButtonColumn 2 ]
                [ viewEditButton division, viewDeleteButton division ]
        ]


viewDeleteButton : Division -> Html Msg
viewDeleteButton division =
    button
        (onClick (DeleteDivisionButtonClick division.id) :: Custom.Attributes.deleteButton)
        [ text "Delete" ]


viewEditButton : Division -> Html Msg
viewEditButton division =
    button
        (onClick (EditDivisionButtonClick division.id) :: Custom.Attributes.editButton)
        [ text "Edit" ]
