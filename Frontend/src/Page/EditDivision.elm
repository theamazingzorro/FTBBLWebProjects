module Page.EditDivision exposing (Model, Msg, init, update, view)

import Api
import Browser.Navigation as Nav
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Division exposing (Division, DivisionId, divisionDecoder, divisionEncoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { navkey : Nav.Key
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


init : Nav.Key -> DivisionId -> ( Model, Cmd Msg )
init navkey id =
    ( { navkey = navkey
      , id = id
      , division = RemoteData.Loading
      , saveError = Nothing
      }
    , getDivisionRequest id
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
            ( { model | saveError = Nothing }, pushUrl model.navkey Route.Divisions )

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
            ( { model | saveError = Nothing }, saveDivision division )

        _ ->
            ( { model | saveError = Just "Cannot submit data, please refresh page and try again." }, Cmd.none )



-- API Requests --


getDivisionRequest : DivisionId -> Cmd Msg
getDivisionRequest id =
    Api.getRequest (Api.Division id) <|
        Http.expectJson (RemoteData.fromResult >> DivisionReceived) divisionDecoder


saveDivision : Division -> Cmd Msg
saveDivision division =
    Api.putRequest (Api.Division division.id)
        (Http.jsonBody (divisionEncoder division))
    <|
        Http.expectJson DivisionSubmitted divisionDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Division" ]
        , br [] []
        , viewSaveError model.saveError
        , viewCoachOrError model
        ]


viewCoachOrError : Model -> Html Msg
viewCoachOrError model =
    case model.division of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success coach ->
            viewDivision coach

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a division at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


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


viewDivision : Division -> Html Msg
viewDivision division =
    div []
        [ viewNameField division.name
        , viewSeasonField division.season
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Save" ]
        ]


viewNameField : String -> Html Msg
viewNameField name =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "nameInput")
            [ text "Name" ]
        , input
            (Custom.Attributes.formInput "nameInput"
                [ onInput NameChanged
                , value name
                ]
            )
            []
        ]


viewSeasonField : Int -> Html Msg
viewSeasonField name =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "seasonInput")
            [ text "Season" ]
        , input
            (Custom.Attributes.formInput "seasonInput"
                [ onInput SeasonChanged
                , onEnter Submit
                , value <| String.fromInt name
                ]
            )
            []
        ]
