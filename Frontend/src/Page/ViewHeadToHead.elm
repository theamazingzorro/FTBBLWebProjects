module Page.ViewHeadToHead exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model.Coach exposing (CoachId)
import Model.Session exposing (Session)
import Model.Team exposing (TeamId)
import RemoteData exposing (WebData)
import Model.Game exposing (Game)
import Model.Team exposing (Team)
import Model.Coach exposing (Coach)



-- Types --


type alias Model =
    { session : Session
    , team1Id : Maybe TeamId
    , team2Id : Maybe TeamId
    , coach1Id : Maybe CoachId
    , coach2Id : Maybe CoachId
    , games : WebData (List Game)
    , team1 : WebData Team
    , team2 : WebData Team
    , coach1 : WebData Coach
    , coach2 : WebData Coach
    }


type Msg
    = None



-- Init --


init : Session -> Maybe ( TeamId, TeamId ) -> Maybe ( CoachId, CoachId ) -> ( Model, Cmd Msg )
init session tData coaches =
    case tData of
        Just (team1, team2) ->
            ( { session = session
                , team1Id = Just team1
                , team2Id = Just team2
                , coach1Id = Nothing
                , coach2Id = Nothing
                , games = RemoteData.Loading
                , team1 = RemoteData.Loading
                , team2 = RemoteData.Loading
                , coach1 = RemoteData.NotAsked
                , coach2 = RemoteData.NotAsked
                }
                , Cmd.none
            )

        Nothing ->
            case coaches of
                Just (coach1, coach2) ->
                    ( { session = session
                        , team1Id = Nothing
                        , team2Id = Nothing
                        , coach1Id = Just coach1
                        , coach2Id = Just coach2
                        , games = RemoteData.Loading
                        , team1 = RemoteData.NotAsked
                        , team2 = RemoteData.NotAsked
                        , coach1 = RemoteData.Loading
                        , coach2 = RemoteData.Loading
                        }
                        , Cmd.none
                    )

                Nothing ->
                    ( { session = session
                        , team1Id = Nothing
                        , team2Id = Nothing
                        , coach1Id = Nothing
                        , coach2Id = Nothing
                        , games = RemoteData.NotAsked
                        , team1 = RemoteData.NotAsked
                        , team2 = RemoteData.NotAsked
                        , coach1 = RemoteData.NotAsked
                        , coach2 = RemoteData.NotAsked
                        }
                        , Cmd.none
                    )


-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )



-- API Requests --
-- View --


view : Model -> Html Msg
view _ =
    text "skeleton"
