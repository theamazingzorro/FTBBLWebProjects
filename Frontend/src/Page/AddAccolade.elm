module Page.AddAccolade exposing (Model, Msg, init, update, view)

import Api
import Custom.Attributes
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Model.Accolade exposing (Accolade, accoladeDecoder, defaultAccolade, newAccoladeEncoder)
import Model.Coach as Coach exposing (Coach, coachsDecoder, defaultCoach)
import Model.Session exposing (Session)
import Model.SharedIds exposing (defaultTeamId)
import Model.Team as Team exposing (Team, teamsDecoder)
import RemoteData exposing (RemoteData(..), WebData)



-- Types --


type alias Model =
    { session : Session
    , accolade : Accolade
    , teams : WebData (List Team)
    , coaches : WebData (List Coach)
    , saveError : Maybe String
    }


type Msg
    = TeamsRecieved (WebData (List Team))
    | CoachesRecieved (WebData (List Coach))
    | Submit
    | AccoladeSubmitted (Result Http.Error Accolade)
    | NameChanged String
    | SeasonChanged String
    | TeamSelected String
    | CoachSelected String
    | ChampBoxChecked
    | RunnerupBoxChecked
    | SidecupBoxChecked



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , accolade = defaultAccolade
      , teams = Loading
      , coaches = Loading
      , saveError = Nothing
      }
    , Cmd.batch
        [ getTeamsRequest session.token
        , getCoachesRequest session.token
        ]
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamsRecieved teams ->
            ( { model | teams = teams }, Cmd.none )

        CoachesRecieved coaches ->
            ( { model | coaches = coaches }, Cmd.none )

        Submit ->
            ( model, saveAccolade model.session.token model.accolade )

        AccoladeSubmitted (Ok _) ->
            ( { model | saveError = Nothing, accolade = defaultAccolade }, Cmd.none )

        AccoladeSubmitted (Err err) ->
            ( { model | saveError = Just (buildErrorMessage err) }, Cmd.none )

        NameChanged newName ->
            let
                rename accolade =
                    { accolade | name = newName }
            in
            ( { model | accolade = rename model.accolade }, Cmd.none )

        SeasonChanged season ->
            let
                reseason accolade =
                    { accolade | season = String.toInt season }
            in
            ( { model | accolade = reseason model.accolade }, Cmd.none )

        TeamSelected teamId ->
            let
                newTeam =
                    getTeamForIdString model.teams teamId

                newCoachId =
                    Maybe.map .coach newTeam
                        |> Maybe.map .id
                        |> Maybe.withDefault model.accolade.coachId

                reteam accolade =
                    { accolade | teamId = Maybe.map .id newTeam, coachId = newCoachId }
            in
            ( { model | accolade = reteam model.accolade }, Cmd.none )

        CoachSelected coachId ->
            let
                newCoachId =
                    getCoachForIdString model.coaches coachId |> .id

                newAccolade accolade =
                    { accolade | coachId = newCoachId, teamId = Nothing }
            in
            ( { model | accolade = newAccolade model.accolade }, Cmd.none )

        ChampBoxChecked ->
            let
                reChamp accolade =
                    { accolade
                        | isChamp = not accolade.isChamp
                        , name =
                            if accolade.isChamp then
                                accolade.name

                            else
                                "Taurus Champs"
                    }
            in
            ( { model | accolade = reChamp model.accolade }, Cmd.none )

        RunnerupBoxChecked ->
            let
                reRunner accolade =
                    { accolade
                        | isRunnerUp = not accolade.isRunnerUp
                        , name =
                            if accolade.isRunnerUp then
                                accolade.name

                            else
                                "Taurus Runner-Up"
                    }
            in
            ( { model | accolade = reRunner model.accolade }, Cmd.none )

        SidecupBoxChecked ->
            let
                reSidecup accolade =
                    { accolade | isSidecup = not accolade.isSidecup }
            in
            ( { model | accolade = reSidecup model.accolade }, Cmd.none )


getTeamForIdString : WebData (List Team) -> String -> Maybe Team
getTeamForIdString data idString =
    case data of
        RemoteData.Success teams ->
            searchByIdString idString Team.idToString teams

        _ ->
            Nothing


getCoachForIdString : WebData (List Coach) -> String -> Coach
getCoachForIdString data idString =
    case data of
        RemoteData.Success coaches ->
            searchByIdString idString Coach.idToString coaches
                |> Maybe.withDefault defaultCoach

        _ ->
            defaultCoach


searchByIdString : String -> (id -> String) -> List { c | id : id } -> Maybe { c | id : id }
searchByIdString idString stringFromId list =
    List.filter (\item -> stringFromId item.id == idString) list
        |> List.head



-- API Requests --


getTeamsRequest : Maybe String -> Cmd Msg
getTeamsRequest token =
    Api.getRequest token Api.Teams <|
        Http.expectJson (RemoteData.fromResult >> TeamsRecieved) teamsDecoder


getCoachesRequest : Maybe String -> Cmd Msg
getCoachesRequest token =
    Api.getRequest token Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder


saveAccolade : Maybe String -> Accolade -> Cmd Msg
saveAccolade token accolade =
    Api.postRequest token
        Api.Accolades
        (Http.jsonBody (newAccoladeEncoder accolade))
        (Http.expectJson AccoladeSubmitted accoladeDecoder)



-- View --


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Add Accolade" ]
        , br [] []
        , viewSaveError model.saveError
        , viewAccolade model model.accolade
        ]


viewSaveError : Maybe String -> Html msg
viewSaveError maybeError =
    case maybeError of
        Just error ->
            div [ Custom.Attributes.errorMessage ]
                [ h3 [] [ text "Couldn't save a accolade at this time." ]
                , text ("Error: " ++ error)
                , br [] []
                ]

        Nothing ->
            text ""


viewAccolade : Model -> Accolade -> Html Msg
viewAccolade model accolade =
    div []
        [ viewDropdown accolade model.teams teamDropdown
        , viewDropdown accolade model.coaches coachDropdown
        , viewNameField accolade.name
        , viewSeasonField accolade.season
        , viewIsChampField accolade.isChamp
        , viewIsRunnerUpField accolade.isRunnerUp
        , viewIsSidecupField accolade.isSidecup
        , button
            [ Custom.Attributes.submitButton
            , onClick Submit
            ]
            [ text "Save" ]
        ]


viewDropdown : Accolade -> WebData (List { a | name : comparable }) -> (Accolade -> List { a | name : comparable } -> Html Msg) -> Html Msg
viewDropdown accolade data dropDownFunction =
    case data of
        RemoteData.NotAsked ->
            dropDownFunction accolade []

        RemoteData.Loading ->
            h4 [] [ text "Loading Options..." ]

        RemoteData.Failure httpError ->
            h4 [ Custom.Attributes.errorMessage ]
                [ text <| "Cannot load Options. " ++ Error.buildErrorMessage httpError ]

        RemoteData.Success d ->
            dropDownFunction accolade <| List.sortBy .name d


teamDropdown : Accolade -> List Team -> Html Msg
teamDropdown accolade teams =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "teamDropdown")
            [ text "Team" ]
        , select
            (Custom.Attributes.formDropdown "teamDropdown"
                [ onInput TeamSelected ]
            )
            (defaultOption :: List.map (teamOption accolade) teams)
        ]


teamOption : Accolade -> Team -> Html msg
teamOption accolade team =
    option
        [ value <| Team.idToString team.id
        , selected (team.id == Maybe.withDefault defaultTeamId accolade.teamId)
        ]
        [ text team.name ]


coachDropdown : Accolade -> List Coach -> Html Msg
coachDropdown accolade coaches =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "coachDropdown")
            [ text "Coach" ]
        , select
            (Custom.Attributes.formDropdown "coachDropdown"
                [ onInput CoachSelected ]
            )
            (defaultOption :: List.map (coachOption accolade) coaches)
        ]


coachOption : Accolade -> Coach -> Html msg
coachOption accolade coach =
    option
        [ value <| Coach.idToString coach.id
        , selected (coach.id == accolade.coachId)
        ]
        [ text coach.name ]


defaultOption : Html Msg
defaultOption =
    option [ value "0" ] [ text "-" ]


viewSeasonField : Maybe Int -> Html Msg
viewSeasonField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "seasonInput")
            [ text "Season" ]
        , input
            (Custom.Attributes.formInput "seasonInput"
                [ onInput SeasonChanged
                , value <| Maybe.withDefault "" <| Maybe.map String.fromInt val
                ]
            )
            []
        ]


viewNameField : String -> Html Msg
viewNameField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "nameInput")
            [ text "Name" ]
        , input
            (Custom.Attributes.formInput "nameInput"
                [ onInput NameChanged
                , value val
                ]
            )
            []
        ]


viewIsChampField : Bool -> Html Msg
viewIsChampField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "champInput")
            [ text "Taurus Champion?" ]
        , input
            (Custom.Attributes.formCheckbox "champInput"
                [ onClick ChampBoxChecked
                , checked val
                ]
            )
            []
        ]


viewIsRunnerUpField : Bool -> Html Msg
viewIsRunnerUpField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "runnerUpInput")
            [ text "Runner up?" ]
        , input
            (Custom.Attributes.formCheckbox "runnerUpInput"
                [ onClick RunnerupBoxChecked
                , checked val
                ]
            )
            []
        ]


viewIsSidecupField : Bool -> Html Msg
viewIsSidecupField val =
    div [ Custom.Attributes.formEntry ]
        [ label
            (Custom.Attributes.formLabel "sidecupInput")
            [ text "Sidecup?" ]
        , input
            (Custom.Attributes.formCheckbox "sidecupInput"
                [ onClick SidecupBoxChecked
                , checked val
                ]
            )
            []
        ]
