module Page exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.AddCoach as AddCoach
import Page.AddTeam as AddTeam
import Page.EditCoach as EditCoach
import Page.EditTeam as EditTeam
import Page.ListCoaches as ListCoaches
import Page.ListTeams as ListTeams
import Route exposing (Route)



-- Types --


type Model
    = NotFoundPage
    | TeamsPage ListTeams.Model
    | AddTeamPage AddTeam.Model
    | EditTeamPage EditTeam.Model
    | CoachesPage ListCoaches.Model
    | AddCoachPage AddCoach.Model
    | EditCoachPage EditCoach.Model


type Msg
    = TeamsPageMsg ListTeams.Msg
    | AddTeamPageMsg AddTeam.Msg
    | EditTeamPageMsg EditTeam.Msg
    | CoachesPageMsg ListCoaches.Msg
    | AddCoachPageMsg AddCoach.Msg
    | EditCoachPageMsg EditCoach.Msg



-- Init --


init : Nav.Key -> Route -> ( Model, Cmd Msg )
init navkey route =
    case route of
        Route.NotFound ->
            ( NotFoundPage, Cmd.none )

        {- Currently points to teams page. -}
        Route.Home ->
            ListTeams.init navkey
                |> wrapWith TeamsPage TeamsPageMsg

        Route.Teams ->
            ListTeams.init navkey
                |> wrapWith TeamsPage TeamsPageMsg

        Route.AddTeam ->
            AddTeam.init
                |> wrapWith AddTeamPage AddTeamPageMsg

        Route.EditTeam id ->
            EditTeam.init navkey id
                |> wrapWith EditTeamPage EditTeamPageMsg

        Route.Coaches ->
            ListCoaches.init navkey
                |> wrapWith CoachesPage CoachesPageMsg

        Route.AddCoach ->
            AddCoach.init
                |> wrapWith AddCoachPage AddCoachPageMsg

        Route.EditCoach id ->
            EditCoach.init navkey id
                |> wrapWith EditCoachPage EditCoachPageMsg



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        {- Team CRUD pages -}
        ( TeamsPageMsg subMsg, TeamsPage pageModel ) ->
            ListTeams.update subMsg pageModel
                |> wrapWith TeamsPage TeamsPageMsg

        ( TeamsPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( AddTeamPageMsg subMsg, AddTeamPage pageModel ) ->
            AddTeam.update subMsg pageModel
                |> wrapWith AddTeamPage AddTeamPageMsg

        ( AddTeamPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditTeamPageMsg subMsg, EditTeamPage pageModel ) ->
            EditTeam.update subMsg pageModel
                |> wrapWith EditTeamPage EditTeamPageMsg

        ( EditTeamPageMsg _, _ ) ->
            ( model, Cmd.none )

        {- Coach CRUD pages -}
        ( CoachesPageMsg subMsg, CoachesPage pageModel ) ->
            ListCoaches.update subMsg pageModel
                |> wrapWith CoachesPage CoachesPageMsg

        ( CoachesPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( AddCoachPageMsg subMsg, AddCoachPage pageModel ) ->
            AddCoach.update subMsg pageModel
                |> wrapWith AddCoachPage AddCoachPageMsg

        ( AddCoachPageMsg _, _ ) ->
            ( model, Cmd.none )

        ( EditCoachPageMsg subMsg, EditCoachPage pageModel ) ->
            EditCoach.update subMsg pageModel
                |> wrapWith EditCoachPage EditCoachPageMsg

        ( EditCoachPageMsg _, _ ) ->
            ( model, Cmd.none )



-- Common helpers --


wrapWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
wrapWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- View --


view : Model -> Html Msg
view model =
    case model of
        NotFoundPage ->
            notFoundView

        TeamsPage pageModel ->
            ListTeams.view pageModel
                |> Html.map TeamsPageMsg

        AddTeamPage pageModel ->
            AddTeam.view pageModel
                |> Html.map AddTeamPageMsg

        EditTeamPage pageModel ->
            EditTeam.view pageModel
                |> Html.map EditTeamPageMsg

        CoachesPage pageModel ->
            ListCoaches.view pageModel
                |> Html.map CoachesPageMsg

        AddCoachPage pageModel ->
            AddCoach.view pageModel
                |> Html.map AddCoachPageMsg

        EditCoachPage pageModel ->
            EditCoach.view pageModel
                |> Html.map EditCoachPageMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]
