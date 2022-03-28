module Page.AddTeam exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Custom.Events exposing (onEnter)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Race as Race exposing (Race, racesDecoder)
import Model.Team exposing (Team, defaultTeam, newTeamEncoder, teamDecoder)
import RemoteData exposing (WebData)



-- Types --


type alias Model =
    { team : Team
    , raceOptions : WebData (List Race)
    , submitError : Maybe String
    }


type Msg
    = FetchRaces
    | RacesRecieved (WebData (List Race))
    | NameChanged String
    | RaceSelected String
    | Submit
    | TeamSubmitted (Result Http.Error Team)



-- Init --


init : ( Model, Cmd Msg )
init =
    ( { team = defaultTeam
      , raceOptions = RemoteData.Loading
      , submitError = Nothing
      }
    , getRacesRequest
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchRaces ->
            ( { model | raceOptions = RemoteData.Loading }, getRacesRequest )

        RacesRecieved response ->
            ( { model | raceOptions = response }, Cmd.none )

        NameChanged newName ->
            let
                rename oldTeam =
                    { oldTeam | name = newName }
            in
            ( { model | team = rename model.team }, Cmd.none )

        RaceSelected idString ->
            let
                newRace =
                    case model.raceOptions of
                        RemoteData.Success races ->
                            searchByIdString idString Race.idToString model.team.race races
                        
                        _ ->
                            model.team.race

                changeRace oldTeam = 
                    { oldTeam | race = newRace }
            in
            ( { model | team = changeRace model.team }, Cmd.none )

        Submit ->
            ( model, submitTeam model.team )

        TeamSubmitted (Ok _) ->
            ( { model | team = defaultTeam, submitError = Nothing }, Cmd.none )

        TeamSubmitted (Err err) ->
            ( { model | submitError = Just (buildErrorMessage err) }, Cmd.none )


searchByIdString : String -> (id -> String) -> { c | id : id } -> List { c | id : id } ->  { c | id : id }
searchByIdString idString stringFromId defaultVal list =
    List.filter (\item -> stringFromId item.id == idString) list
        |> List.head
        |> Maybe.withDefault defaultVal


-- API Requests --


submitTeam : Team -> Cmd Msg
submitTeam team =
    Api.postRequest Api.Teams
        (Http.jsonBody (newTeamEncoder team))
    <|
        Http.expectJson TeamSubmitted teamDecoder


getRacesRequest : Cmd Msg
getRacesRequest =
    Api.getRequest Api.Races <|
        Http.expectJson (RemoteData.fromResult >> RacesRecieved) racesDecoder



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Add Team" ]
        , br [] []
        , viewSubmitError model.submitError
        , viewForm model
        ]


viewSubmitError : Maybe String -> Html msg
viewSubmitError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a team at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewForm : Model -> Html Msg
viewForm model =
    div []
        [ viewNameField model.team
        , viewRaceField model.raceOptions
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Add" ]
        ]


viewNameField : Team -> Html Msg
viewNameField team =
    div [ Custom.Attributes.formEntry ]
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


viewRaceField : WebData (List Race) -> Html Msg
viewRaceField data =
    case data of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h4 [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            h4 [ Custom.Attributes.errorMessage ] [ text <| "Cannot load options. " ++ Error.buildErrorMessage httpError]

        RemoteData.Success races ->
            raceDropdown races


raceDropdown : List Race -> Html Msg
raceDropdown races =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "raceDropdown")
            [ text "Race" ]
        , select
            (Custom.Attributes.formDropdown "raceDropdown" 
                [ onInput RaceSelected ]
            )
            (defaultOption :: (races |> List.map raceOption))
        ]


defaultOption : Html Msg
defaultOption =
    option [ value "0" ] [ text "-" ]


raceOption : Race -> Html msg
raceOption race =
    option [ value <| Race.idToString race.id ] [ text race.name ]
