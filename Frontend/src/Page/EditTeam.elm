module Page.EditTeam exposing (Model, Msg, init, update, view)

import Api exposing (Endpoint(..))
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Coach exposing (CoachId)
import Model.Session exposing (Session)
import Model.Team exposing (Team, TeamId, teamDecoder, teamEncoder)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)



-- Types --


type alias Model =
    { session : Session
    , id : TeamId
    , team : WebData Team
    , saveError : Maybe String
    }


type Msg
    = TeamReceived (WebData Team)
    | NameChanged String
    | CoachLinkClicked CoachId
    | Submit
    | TeamSubmitted (Result Http.Error Team)



-- Init --


init : Session -> TeamId -> ( Model, Cmd Msg )
init session id =
    ( { session = session
      , id = id
      , team = RemoteData.Loading
      , saveError = Nothing
      }
    , getTeamRequest session.token id
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamReceived response ->
            ( { model | team = response }, Cmd.none )

        NameChanged newName ->
            ( { model | team = rename model.team newName }, Cmd.none )

        CoachLinkClicked id ->
            ( model, pushUrl model.session.navkey <| Route.EditCoach id )

        Submit ->
            trySaveTeam model

        TeamSubmitted (Ok _) ->
            ( { model | saveError = Nothing }, pushUrl model.session.navkey Route.Teams )

        TeamSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )


rename : WebData Team -> String -> WebData Team
rename team newName =
    case team of
        RemoteData.Success oldTeam ->
            RemoteData.Success { oldTeam | name = newName }

        _ ->
            team


trySaveTeam : Model -> ( Model, Cmd Msg )
trySaveTeam model =
    case model.team of
        RemoteData.Success team ->
            ( { model | saveError = Nothing }, saveTeam model.session.token team )

        _ ->
            ( { model | saveError = Just "Cannot submit data, please refresh page and try again." }, Cmd.none )



-- API Requests --


getTeamRequest : Maybe String -> TeamId -> Cmd Msg
getTeamRequest token id =
    Api.getRequest token (Api.Team id) <|
        Http.expectJson (RemoteData.fromResult >> TeamReceived) teamDecoder


saveTeam : Maybe String -> Team -> Cmd Msg
saveTeam token team =
    Api.putRequest token
        (Api.Team team.id)
        (Http.jsonBody (teamEncoder team))
    <|
        Http.expectJson TeamSubmitted teamDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Edit Team" ]
        , br [] []
        , viewSaveError model.saveError
        , viewTeamOrError model
        ]


viewTeamOrError : Model -> Html Msg
viewTeamOrError model =
    case model.team of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success team ->
            viewTeam team

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a team at this time." ]
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


viewTeam : Team -> Html Msg
viewTeam team =
    div []
        [ viewNameField team.name
        , viewStaticField "raceField" "Race" team.race.name
        , viewLinkedStaticField "coachField" "Coach" team.coach.name <| CoachLinkClicked team.coach.id
        , viewStaticField "eloField" "Elo" <| String.fromInt team.elo
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


viewLinkedStaticField : String -> String -> String -> msg -> Html msg
viewLinkedStaticField id lblText entry msg =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel id)
            [ text lblText ]
        , input
            (Custom.Attributes.formInputLink id
                msg
                [ readonly True
                , value entry
                ]
            )
            []
        ]
