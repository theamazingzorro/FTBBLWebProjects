module Page.AddTeam exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Team exposing (Team, defaultTeam, newTeamEncoder, teamDecoder)



-- Types --


type alias Model =
    { team : Team
    , submitError : Maybe String
    }


type Msg
    = NameChanged String
    | Submit
    | TeamSubmitted (Result Http.Error Team)



-- Init --


init : ( Model, Cmd Msg )
init =
    ( { team = defaultTeam, submitError = Nothing }, Cmd.none )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameChanged newName ->
            let
                rename oldTeam =
                    { oldTeam | name = newName }
            in
            ( { model | team = rename model.team }, Cmd.none )

        Submit ->
            ( model, submitTeam model.team )

        TeamSubmitted (Ok _) ->
            ( { model | team = defaultTeam, submitError = Nothing }, Cmd.none )

        TeamSubmitted (Err err) ->
            ( { model | submitError = Just (buildErrorMessage err) }, Cmd.none )


submitTeam : Team -> Cmd Msg
submitTeam team =
    Api.postRequest Api.Teams
        (Http.jsonBody (newTeamEncoder team))
    <|
        Http.expectJson TeamSubmitted teamDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Add Team" ]
        , br [] []
        , viewError model.submitError
        , viewForm model.team
        ]


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a team at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewForm : Team -> Html Msg
viewForm team =
    div []
        [ div [ Custom.Attributes.formEntry ]
            [ label
                (Custom.Attributes.formLabel "nameInput")
                [ text "Name" ]
            , input
                (Custom.Attributes.formInput "nameInput"
                    [ onInput NameChanged
                    , onEnter Submit
                    , value team.name
                    ]
                )
                []
            ]
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Add" ]
        ]
