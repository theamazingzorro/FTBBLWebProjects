module Page.EditDivision exposing (Model, Msg, init, update, view)

import Api
import Custom.Events exposing (onEnter)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Http
import Model.Division exposing (Division, DivisionId, divisionDecoder, divisionEncoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { session : Session
    , id : DivisionId
    , division : WebData Division
    , saveError : Maybe String
    }


type Msg
    = DivisionReceived (WebData Division)
    | NameChanged String
    | SeasonChanged String
    | Submit
    | DivisionSubmitted (Result Http.Error Division)



-- Init --


init : Session -> DivisionId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , division = RemoteData.Loading
      , saveError = Nothing
      }
    , getDivisionRequest session.token id
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DivisionReceived response ->
            ( { model | division = response }, Cmd.none )

        NameChanged newName ->
            ( { model | division = rename model.division newName }, Cmd.none )

        SeasonChanged newSeason ->
            ( { model | division = reseason model.division newSeason }, Cmd.none )

        Submit ->
            trySaveDivision model

        DivisionSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, pushUrl model.session.navkey Route.Divisions )

        DivisionSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )


rename : WebData Division -> String -> WebData Division
rename division newName =
    case division of
        RemoteData.Success oldDivision ->
            RemoteData.Success { oldDivision | name = newName }

        _ ->
            division


reseason : WebData Division -> String -> WebData Division
reseason division newSeasonText =
    case division of
        RemoteData.Success oldDivision ->
            let
                newSeason =
                    String.toInt newSeasonText
                        |> Maybe.withDefault 0
            in
            RemoteData.Success { oldDivision | season = newSeason }

        _ ->
            division


trySaveDivision : Model -> ( Model, Cmd Msg )
trySaveDivision model =
    case model.division of
        RemoteData.Success division ->
            ( { model | saveError = Nothing }, saveDivision model.session.token division )

        _ ->
            ( { model | saveError = Just "Cannot submit data, please refresh page and try again." }, Cmd.none )



-- API Requests --


getDivisionRequest : Maybe String -> DivisionId -> Cmd Msg
getDivisionRequest token id =
    Api.getRequest token (Api.Division id) <|
        Http.expectJson (RemoteData.fromResult >> DivisionReceived) divisionDecoder


saveDivision : Maybe String -> Division -> Cmd Msg
saveDivision token division =
    Api.putRequest token
        (Api.Division division.id)
        (Http.jsonBody (divisionEncoder division))
    <|
        Http.expectJson DivisionSubmitted divisionDecoder



-- View --


view : Model -> Html Msg
view model =
    row []
        [ mainHeader [] [ text "Edit Division" ]
        , viewSaveError model.saveError
        , viewCoachOrError model
        ]


viewCoachOrError : Model -> Html Msg
viewCoachOrError model =
    case model.division of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success coach ->
            viewDivision coach

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            errorText []
                [ emphasisText [] [ text "Couldn't save a division at this time." ]
                , text <| "Error: " ++ error
                ]

        Nothing ->
            text ""


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    errorText []
        [ emphasisText [] [ text "Couldn't fetch data at this time." ]
        , text <| "Error: " ++ errorMessage
        ]


viewDivision : Division -> Html Msg
viewDivision division =
    inputForm []
        [ viewNameField division.name
        , viewSeasonField division.season
        , submitButton Submit [ text "Save" ]
        ]


viewNameField : String -> Html Msg
viewNameField name =
    textInput
        [ onInput NameChanged
        , value name
        ]
        [ text "Name" ]


viewSeasonField : Int -> Html Msg
viewSeasonField name =
    textInput
        [ onInput SeasonChanged
        , onEnter Submit
        , value <| String.fromInt name
        ]
        [ text "Season" ]
