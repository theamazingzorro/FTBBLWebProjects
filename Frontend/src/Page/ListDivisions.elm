module Page.ListDivisions exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Custom.Attributes
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Division exposing (Division, DivisionId, divisionsDecoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { divisions : WebData (List Division)
    , navkey : Nav.Key
    , deleteError : Maybe String
    }


type Msg
    = FetchDivisions
    | DivisionsRecieved (WebData (List Division))
    | AddDivisionButtonClick
    | EditDivisionButtonClick DivisionId
    | DeleteDivisionButtonClick DivisionId
    | DivisionDeleted (Result Http.Error DeleteResponse)



-- Init --


init : Nav.Key -> ( Model, Cmd Msg )
init navkey =
    ( { divisions = RemoteData.Loading
      , navkey = navkey
      , deleteError = Nothing
      }
    , getDivisionsRequest
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDivisions ->
            ( { model | divisions = RemoteData.Loading }, getDivisionsRequest )

        DivisionsRecieved response ->
            ( { model | divisions = response }, Cmd.none )

        AddDivisionButtonClick ->
            ( model, pushUrl model.navkey Route.AddDivision )

        EditDivisionButtonClick id ->
            ( model, pushUrl model.navkey <| Route.EditDivision id )

        DeleteDivisionButtonClick id ->
            ( model, deleteDivisionRequest id )

        DivisionDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getDivisionsRequest )

        DivisionDeleted (Err err) ->
            ( { model | deleteError = Just (Error.buildErrorMessage err) }, Cmd.none )


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Does the division still have teams in it?"



-- API Requests --


getDivisionsRequest : Cmd Msg
getDivisionsRequest =
    Api.getRequest Api.Divisions <|
        Http.expectJson (RemoteData.fromResult >> DivisionsRecieved) divisionsDecoder


deleteDivisionRequest : DivisionId -> Cmd Msg
deleteDivisionRequest id =
    Api.deleteRequest (Api.Division id) <|
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
            viewDivisions divisions

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

viewDivisions : List Division -> Html Msg
viewDivisions divisions =
    div []
        [ viewHeader
        , table [ Custom.Attributes.table ]
            [ viewTableHeader
            , tbody [] <|
                List.map viewDivision divisions
            ]
        ]


viewHeader : Html Msg
viewHeader =
    div Custom.Attributes.row
        [ div [ Custom.Attributes.col ] [ h3 [] [ text "Divisions" ] ]
        , div [ Custom.Attributes.col ] [ viewToolBar ]
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


viewDivision : Division -> Html Msg
viewDivision division =
    tr []
        [ td []
            [ text division.name ]
        , td []
            [ text <| String.fromInt division.season ]
        , td [ Custom.Attributes.tableButtonColumn 2 ]
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
