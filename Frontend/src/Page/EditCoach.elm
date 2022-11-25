module Page.EditCoach exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Coach exposing (Coach, CoachId, coachDecoder, coachEncoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { session : Session
    , id : CoachId
    , coach : WebData Coach
    , saveError : Maybe String
    }


type Msg
    = CoachReceived (WebData Coach)
    | NameChanged String
    | Submit
    | CoachSubmitted (Result Http.Error Coach)



-- Init --


init : Session -> CoachId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , coach = RemoteData.Loading
      , saveError = Nothing
      }
    , getCoachRequest session.token id
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CoachReceived response ->
            ( { model | coach = response }, Cmd.none )

        NameChanged newName ->
            ( { model | coach = rename model.coach newName }, Cmd.none )

        Submit ->
            trySaveCoach model

        CoachSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, pushUrl model.session.navkey Route.Coaches )

        CoachSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )


rename : WebData Coach -> String -> WebData Coach
rename coach newName =
    case coach of
        RemoteData.Success oldCoach ->
            RemoteData.Success { oldCoach | name = newName }

        _ ->
            coach


trySaveCoach : Model -> ( Model, Cmd Msg )
trySaveCoach model =
    case model.coach of
        RemoteData.Success coach ->
            ( { model | saveError = Nothing }, saveCoach model.session.token coach )

        _ ->
            ( { model | saveError = Just "Cannot submit data, please refresh page and try again." }, Cmd.none )



-- API Requests --


getCoachRequest : Maybe String -> CoachId -> Cmd Msg
getCoachRequest token id =
    Api.getRequest token (Api.Coach id) <|
        Http.expectJson (RemoteData.fromResult >> CoachReceived) coachDecoder


saveCoach : Maybe String -> Coach -> Cmd Msg
saveCoach token coach =
    Api.putRequest token
        (Api.Coach coach.id)
        (Http.jsonBody (coachEncoder coach))
    <|
        Http.expectJson CoachSubmitted coachDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Coach" ]
        , br [] []
        , viewSaveError model.saveError
        , viewCoachOrError model
        ]


viewCoachOrError : Model -> Html Msg
viewCoachOrError model =
    case model.coach of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success coach ->
            viewCoach coach

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a coach at this time." ]
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


viewCoach : Coach -> Html Msg
viewCoach coach =
    div []
        [ viewNameField coach.name
        , viewStaticField "eloField" "Elo" <| String.fromInt coach.elo
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
                , onEnter Submit
                , value name
                ]
            )
            []
        ]


viewStaticField : String -> String -> String -> Html msg
viewStaticField id lblText entry =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel id)
            [ text lblText ]
        , input
            (Custom.Attributes.formInput id
                [ readonly True
                , value entry
                ]
            )
            []
        ]
