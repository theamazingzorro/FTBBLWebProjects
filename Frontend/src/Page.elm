module Page exposing (Model, Msg, OutMsg(..), init, update, view)

import Custom.Html exposing (emphasisText, mainHeader, row)
import Html exposing (Html, text)
import Html.Attributes exposing (..)
import Model.Session exposing (Session)
import Page.AddAccolade as AddAccolade
import Page.AddCoach as AddCoach
import Page.AddDivision as AddDivision
import Page.AddGame as AddGame
import Page.AddGameWeek as AddGameWeek
import Page.AddTeam as AddTeam
import Page.AddTeamToDiv as AddTeamToDiv
import Page.EditCoach as EditCoach
import Page.EditDivision as EditDivision
import Page.EditGame as EditGame
import Page.EditTeam as EditTeam
import Page.ListAccolades as ListAccolades
import Page.ListCoaches as ListCoaches
import Page.ListDivisions as ListDivisions
import Page.ListTeams as ListTeams
import Page.Signin as Signin
import Page.ViewCoach as ViewCoach
import Page.ViewDivision as ViewDivision
import Page.ViewHeadToHead as ViewHeadToHead
import Page.ViewTeam as ViewTeam
import Route exposing (Route(..))



-- Types --


type Model
    = NotFoundPage
    | SigninPage Signin.Model
    | TeamsPage ListTeams.Model
    | AddTeamPage AddTeam.Model
    | EditTeamPage EditTeam.Model
    | ViewTeamPage ViewTeam.Model
    | CoachesPage ListCoaches.Model
    | AddCoachPage AddCoach.Model
    | EditCoachPage EditCoach.Model
    | ViewCoachPage ViewCoach.Model
    | DivisionsPage ListDivisions.Model
    | AddDivisionPage AddDivision.Model
    | EditDivisionPage EditDivision.Model
    | AddGamePage AddGame.Model
    | EditGamePage EditGame.Model
    | ViewDivisionPage ViewDivision.Model
    | AddTeamToDivPage AddTeamToDiv.Model
    | AddGameWeekPage AddGameWeek.Model
    | AccoladesPage ListAccolades.Model
    | AddAccoladePage AddAccolade.Model
    | ViewHeadToHeadPage ViewHeadToHead.Model


type Msg
    = SigninPageMsg Signin.Msg
    | TeamsPageMsg ListTeams.Msg
    | AddTeamPageMsg AddTeam.Msg
    | EditTeamPageMsg EditTeam.Msg
    | ViewTeamPageMsg ViewTeam.Msg
    | CoachesPageMsg ListCoaches.Msg
    | AddCoachPageMsg AddCoach.Msg
    | EditCoachPageMsg EditCoach.Msg
    | ViewCoachPageMsg ViewCoach.Msg
    | DivisionsPageMsg ListDivisions.Msg
    | AddDivisionPageMsg AddDivision.Msg
    | EditDivisionPageMsg EditDivision.Msg
    | AddGamePageMsg AddGame.Msg
    | EditGamePageMsg EditGame.Msg
    | ViewDivisionPageMsg ViewDivision.Msg
    | AddTeamToDivPageMsg AddTeamToDiv.Msg
    | AddGameWeekMsg AddGameWeek.Msg
    | AccoladesPageMsg ListAccolades.Msg
    | AddAccoladePageMsg AddAccolade.Msg
    | ViewHeadToHeadPageMsg ViewHeadToHead.Msg


type OutMsg
    = SigninPageOutMsg Signin.OutMsg



-- Init --


init : Session -> Route -> ( Model, Cmd Msg )
init session route =
    case route of
        Route.NotFound ->
            ( NotFoundPage, Cmd.none )

        {- Currently points to teams page. -}
        Route.Home ->
            ListTeams.init session
                |> wrapInitWith TeamsPage TeamsPageMsg

        Route.Signin ->
            Signin.init session
                |> wrapInitWith SigninPage SigninPageMsg

        Route.Teams ->
            ListTeams.init session
                |> wrapInitWith TeamsPage TeamsPageMsg

        Route.AddTeam ->
            AddTeam.init session
                |> wrapInitWith AddTeamPage AddTeamPageMsg
                |> requiresAuth session

        Route.EditTeam id ->
            EditTeam.init session id
                |> wrapInitWith EditTeamPage EditTeamPageMsg
                |> requiresAuth session

        Route.ViewTeam id ->
            ViewTeam.init session id
                |> wrapInitWith ViewTeamPage ViewTeamPageMsg

        Route.Coaches ->
            ListCoaches.init session
                |> wrapInitWith CoachesPage CoachesPageMsg

        Route.AddCoach ->
            AddCoach.init session
                |> wrapInitWith AddCoachPage AddCoachPageMsg
                |> requiresAuth session

        Route.EditCoach id ->
            EditCoach.init session id
                |> wrapInitWith EditCoachPage EditCoachPageMsg
                |> requiresAuth session

        Route.ViewCoach id ->
            ViewCoach.init session id
                |> wrapInitWith ViewCoachPage ViewCoachPageMsg

        Route.Divisions ->
            ListDivisions.init session
                |> wrapInitWith DivisionsPage DivisionsPageMsg

        Route.AddDivision ->
            AddDivision.init session
                |> wrapInitWith AddDivisionPage AddDivisionPageMsg
                |> requiresAuth session

        Route.EditDivision id ->
            EditDivision.init session id
                |> wrapInitWith EditDivisionPage EditDivisionPageMsg
                |> requiresAuth session

        Route.AddGame ->
            AddGame.init session Nothing Nothing
                |> wrapInitWith AddGamePage AddGamePageMsg
                |> requiresAuth session

        Route.AddGameWithDefaults divId week ->
            AddGame.init session (Just divId) (Just week)
                |> wrapInitWith AddGamePage AddGamePageMsg
                |> requiresAuth session

        Route.EditGame id ->
            EditGame.init session id
                |> wrapInitWith EditGamePage EditGamePageMsg
                |> requiresAuth session

        Route.ViewDivision id ->
            ViewDivision.init session id Nothing
                |> wrapInitWith ViewDivisionPage ViewDivisionPageMsg

        Route.ViewDivisionWeek id week ->
            ViewDivision.init session id (Just week)
                |> wrapInitWith ViewDivisionPage ViewDivisionPageMsg

        Route.AddTeamToDivision id ->
            AddTeamToDiv.init session id
                |> wrapInitWith AddTeamToDivPage AddTeamToDivPageMsg
                |> requiresAuth session

        Route.AddGameWeek id week ->
            AddGameWeek.init session id week
                |> wrapInitWith AddGameWeekPage AddGameWeekMsg
                |> requiresAuth session

        Route.Accolades ->
            ListAccolades.init session
                |> wrapInitWith AccoladesPage AccoladesPageMsg
                |> requiresAuth session

        Route.AddAccolade ->
            AddAccolade.init session Nothing Nothing
                |> wrapInitWith AddAccoladePage AddAccoladePageMsg
                |> requiresAuth session

        Route.AddAccoladeWithDefaults teamId coachId ->
            AddAccolade.init session teamId (Just coachId)
                |> wrapInitWith AddAccoladePage AddAccoladePageMsg
                |> requiresAuth session

        Route.ViewHeadToHeadDefault ->
            ViewHeadToHead.init session Nothing Nothing
                |> wrapInitWith ViewHeadToHeadPage ViewHeadToHeadPageMsg

        Route.ViewHeadToHeadTeams team1Id team2Id ->
            ViewHeadToHead.init session (Just ( team1Id, team2Id )) Nothing
                |> wrapInitWith ViewHeadToHeadPage ViewHeadToHeadPageMsg

        Route.ViewHeadToHeadCoaches coach1Id coach2Id ->
            ViewHeadToHead.init session Nothing (Just ( coach1Id, coach2Id ))
                |> wrapInitWith ViewHeadToHeadPage ViewHeadToHeadPageMsg



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update msg model =
    case ( msg, model ) of
        ( SigninPageMsg subMsg, SigninPage pageModel ) ->
            Signin.update subMsg pageModel
                |> wrapUpdateWithOutMsg SigninPage SigninPageMsg SigninPageOutMsg

        ( SigninPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        {- Team CRUD pages -}
        ( TeamsPageMsg subMsg, TeamsPage pageModel ) ->
            ListTeams.update subMsg pageModel
                |> wrapUpdateWith TeamsPage TeamsPageMsg

        ( TeamsPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( AddTeamPageMsg subMsg, AddTeamPage pageModel ) ->
            AddTeam.update subMsg pageModel
                |> wrapUpdateWith AddTeamPage AddTeamPageMsg

        ( AddTeamPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( EditTeamPageMsg subMsg, EditTeamPage pageModel ) ->
            EditTeam.update subMsg pageModel
                |> wrapUpdateWith EditTeamPage EditTeamPageMsg

        ( EditTeamPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( ViewTeamPageMsg subMsg, ViewTeamPage pageModel ) ->
            ViewTeam.update subMsg pageModel
                |> wrapUpdateWith ViewTeamPage ViewTeamPageMsg

        ( ViewTeamPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        {- Coach CRUD pages -}
        ( CoachesPageMsg subMsg, CoachesPage pageModel ) ->
            ListCoaches.update subMsg pageModel
                |> wrapUpdateWith CoachesPage CoachesPageMsg

        ( CoachesPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( AddCoachPageMsg subMsg, AddCoachPage pageModel ) ->
            AddCoach.update subMsg pageModel
                |> wrapUpdateWith AddCoachPage AddCoachPageMsg

        ( AddCoachPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( EditCoachPageMsg subMsg, EditCoachPage pageModel ) ->
            EditCoach.update subMsg pageModel
                |> wrapUpdateWith EditCoachPage EditCoachPageMsg

        ( EditCoachPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( ViewCoachPageMsg subMsg, ViewCoachPage pageModel ) ->
            ViewCoach.update subMsg pageModel
                |> wrapUpdateWith ViewCoachPage ViewCoachPageMsg

        ( ViewCoachPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        {- Division CRUD pages -}
        ( DivisionsPageMsg subMsg, DivisionsPage pageModel ) ->
            ListDivisions.update subMsg pageModel
                |> wrapUpdateWith DivisionsPage DivisionsPageMsg

        ( DivisionsPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( AddDivisionPageMsg subMsg, AddDivisionPage pageModel ) ->
            AddDivision.update subMsg pageModel
                |> wrapUpdateWith AddDivisionPage AddDivisionPageMsg

        ( AddDivisionPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( EditDivisionPageMsg subMsg, EditDivisionPage pageModel ) ->
            EditDivision.update subMsg pageModel
                |> wrapUpdateWith EditDivisionPage EditDivisionPageMsg

        ( EditDivisionPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        {- Game CRUD pages -}
        ( AddGamePageMsg subMsg, AddGamePage pageModel ) ->
            AddGame.update subMsg pageModel
                |> wrapUpdateWith AddGamePage AddGamePageMsg

        ( AddGamePageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( EditGamePageMsg subMsg, EditGamePage pageModel ) ->
            EditGame.update subMsg pageModel
                |> wrapUpdateWith EditGamePage EditGamePageMsg

        ( EditGamePageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        {- Accolade CRUD pages -}
        ( AccoladesPageMsg subMsg, AccoladesPage pageModel ) ->
            ListAccolades.update subMsg pageModel
                |> wrapUpdateWith AccoladesPage AccoladesPageMsg

        ( AccoladesPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( AddAccoladePageMsg subMsg, AddAccoladePage pageModel ) ->
            AddAccolade.update subMsg pageModel
                |> wrapUpdateWith AddAccoladePage AddAccoladePageMsg

        ( AddAccoladePageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        {- Other -}
        ( AddTeamToDivPageMsg subMsg, AddTeamToDivPage pageModel ) ->
            AddTeamToDiv.update subMsg pageModel
                |> wrapUpdateWith AddTeamToDivPage AddTeamToDivPageMsg

        ( AddTeamToDivPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( AddGameWeekMsg subMsg, AddGameWeekPage pageModel ) ->
            AddGameWeek.update subMsg pageModel
                |> wrapUpdateWith AddGameWeekPage AddGameWeekMsg

        ( AddGameWeekMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( ViewDivisionPageMsg subMsg, ViewDivisionPage pageModel ) ->
            ViewDivision.update subMsg pageModel
                |> wrapUpdateWith ViewDivisionPage ViewDivisionPageMsg

        ( ViewDivisionPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )

        ( ViewHeadToHeadPageMsg subMsg, ViewHeadToHeadPage pageModel ) ->
            ViewHeadToHead.update subMsg pageModel
                |> wrapUpdateWith ViewHeadToHeadPage ViewHeadToHeadPageMsg

        ( ViewHeadToHeadPageMsg _, _ ) ->
            ( model, Cmd.none, Nothing )



-- Common helpers --


wrapUpdateWithOutMsg : (subModel -> Model) -> (subMsg -> Msg) -> (subOut -> OutMsg) -> ( subModel, Cmd subMsg, Maybe subOut ) -> ( Model, Cmd Msg, Maybe OutMsg )
wrapUpdateWithOutMsg toModel toMsg toOutMsg ( subModel, subCmd, subOut ) =
    case subOut of
        Just subOutMsg ->
            ( toModel subModel
            , Cmd.map toMsg subCmd
            , Just (toOutMsg subOutMsg)
            )

        Nothing ->
            ( toModel subModel
            , Cmd.map toMsg subCmd
            , Nothing
            )


wrapUpdateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg, Maybe OutMsg )
wrapUpdateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    , Nothing
    )


wrapInitWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
wrapInitWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


requiresAuth : Session -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
requiresAuth session ( model, cmd ) =
    case session.token of
        Nothing ->
            ( NotFoundPage, Cmd.none )

        Just _ ->
            ( model, cmd )



-- View --


view : Model -> Html Msg
view model =
    case model of
        NotFoundPage ->
            notFoundView

        SigninPage pageModel ->
            Signin.view pageModel
                |> Html.map SigninPageMsg

        TeamsPage pageModel ->
            ListTeams.view pageModel
                |> Html.map TeamsPageMsg

        AddTeamPage pageModel ->
            AddTeam.view pageModel
                |> Html.map AddTeamPageMsg

        EditTeamPage pageModel ->
            EditTeam.view pageModel
                |> Html.map EditTeamPageMsg

        ViewTeamPage pageModel ->
            ViewTeam.view pageModel
                |> Html.map ViewTeamPageMsg

        CoachesPage pageModel ->
            ListCoaches.view pageModel
                |> Html.map CoachesPageMsg

        AddCoachPage pageModel ->
            AddCoach.view pageModel
                |> Html.map AddCoachPageMsg

        EditCoachPage pageModel ->
            EditCoach.view pageModel
                |> Html.map EditCoachPageMsg

        ViewCoachPage pageModel ->
            ViewCoach.view pageModel
                |> Html.map ViewCoachPageMsg

        DivisionsPage pageModel ->
            ListDivisions.view pageModel
                |> Html.map DivisionsPageMsg

        AddDivisionPage pageModel ->
            AddDivision.view pageModel
                |> Html.map AddDivisionPageMsg

        EditDivisionPage pageModel ->
            EditDivision.view pageModel
                |> Html.map EditDivisionPageMsg

        AddGamePage pageModel ->
            AddGame.view pageModel
                |> Html.map AddGamePageMsg

        EditGamePage pageModel ->
            EditGame.view pageModel
                |> Html.map EditGamePageMsg

        ViewDivisionPage pageModel ->
            ViewDivision.view pageModel
                |> Html.map ViewDivisionPageMsg

        AddTeamToDivPage pageModel ->
            AddTeamToDiv.view pageModel
                |> Html.map AddTeamToDivPageMsg

        AddGameWeekPage pageModel ->
            AddGameWeek.view pageModel
                |> Html.map AddGameWeekMsg

        AccoladesPage pageModel ->
            ListAccolades.view pageModel
                |> Html.map AccoladesPageMsg

        AddAccoladePage pageModel ->
            AddAccolade.view pageModel
                |> Html.map AddAccoladePageMsg

        ViewHeadToHeadPage pageModel ->
            ViewHeadToHead.view pageModel
                |> Html.map ViewHeadToHeadPageMsg


notFoundView : Html msg
notFoundView =
    row []
        [ mainHeader [] [ text "Oops!" ]
        , emphasisText [] [ text "The page you requested was not found!" ]
        ]
