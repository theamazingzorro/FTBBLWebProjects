module Page.ListCoaches exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Html exposing (..)
import Error exposing (buildErrorMessage)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Http
import List exposing (drop, length, take)
import Model.Accolade exposing (viewAccolade)
import Model.Coach exposing (Coach, CoachId, coachsDecoder)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import String exposing (toLower)



-- Types --


type alias Model =
    { coaches : WebData (List Coach)
    , page : Int
    , sortingMethod : SortingMethod
    , session : Session
    , deleteError : Maybe String
    }


type Msg
    = FetchCoaches
    | CoachesRecieved (WebData (List Coach))
    | AddCoachButtonClick
    | DeleteCoachButtonClick CoachId
    | EditCoachButtonClick CoachId
    | ViewCoachClick CoachId
    | CoachDeleted (Result Http.Error DeleteResponse)
    | NameSortClick
    | EloSortClick
    | SeasonSortClick
    | FirstPageClick
    | PrevPageClick
    | NextPageClick
    | LastPageClick


type SortingMethod
    = Default
    | Name
    | NameDesc
    | Elo
    | EloDesc
    | Season
    | SeasonDesc



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { coaches = RemoteData.Loading
      , page = 0
      , sortingMethod = Default
      , session = session
      , deleteError = Nothing
      }
    , getCoachesRequest session.token
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchCoaches ->
            ( { model | coaches = RemoteData.Loading }, getCoachesRequest model.session.token )

        CoachesRecieved response ->
            ( { model | coaches = response }, Cmd.none )

        AddCoachButtonClick ->
            ( model, pushUrl model.session.navkey Route.AddCoach )

        EditCoachButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditCoach id )

        ViewCoachClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewCoach id )

        DeleteCoachButtonClick id ->
            ( model, deleteCoachRequest model.session.token id )

        CoachDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getCoachesRequest model.session.token )

        CoachDeleted (Err err) ->
            ( { model | deleteError = Just (buildErrorMessage err) }, Cmd.none )

        NameSortClick ->
            ( { model | sortingMethod = newSort Name NameDesc model.sortingMethod }, Cmd.none )

        EloSortClick ->
            ( { model | sortingMethod = newSort EloDesc Elo model.sortingMethod }, Cmd.none )

        SeasonSortClick ->
            ( { model | sortingMethod = newSort SeasonDesc Season model.sortingMethod }, Cmd.none )

        FirstPageClick ->
            ( { model | page = 0 }, Cmd.none )

        PrevPageClick ->
            ( { model | page = Basics.max 0 (model.page - 1) }, Cmd.none )

        NextPageClick ->
            ( { model | page = Basics.min (lastPage model.coaches) (model.page + 1) }, Cmd.none )

        LastPageClick ->
            ( { model | page = lastPage model.coaches }, Cmd.none )


newSort : SortingMethod -> SortingMethod -> SortingMethod -> SortingMethod
newSort default alt oldSort =
    if oldSort == default then
        alt

    else
        default


buildDeleteError : DeleteResponse -> Maybe String
buildDeleteError res =
    if res.deleted then
        Nothing

    else
        Just "Delete Failed. Coaches cannot be deleted before their last team."



-- API Requests --


getCoachesRequest : Maybe String -> Cmd Msg
getCoachesRequest token =
    Api.getRequest token Api.Coaches <|
        Http.expectJson (RemoteData.fromResult >> CoachesRecieved) coachsDecoder


deleteCoachRequest : Maybe String -> CoachId -> Cmd Msg
deleteCoachRequest token id =
    Api.deleteRequest token (Api.Coach id) <|
        Http.expectJson CoachDeleted deleteResponseDecoder



-- Helper Functions --


sortedCoaches : SortingMethod -> List Coach -> List Coach
sortedCoaches sortingMethod coaches =
    let
        compareStrIgnoreCase a b =
            compare (toLower a) (toLower b)

        compareSeason a b =
            case a.recentSeason of
                Just aSeason ->
                    case b.recentSeason of
                        Just bSeason ->
                            compare aSeason bSeason

                        Nothing ->
                            GT

                Nothing ->
                    case b.recentSeason of
                        Just _ ->
                            LT

                        Nothing ->
                            EQ

        secondarySortElo primarySort a b =
            case primarySort a b of
                EQ ->
                    compare b.elo a.elo

                other ->
                    other

        compareElo a b =
            compare a.elo b.elo

        compareName a b =
            compareStrIgnoreCase a.name b.name

        reverse func a b =
            func b a
    in
    case sortingMethod of
        Default ->
            List.sortWith (reverse compareElo) coaches

        Name ->
            List.sortWith compareName coaches

        NameDesc ->
            List.sortWith (reverse compareName) coaches

        Elo ->
            List.sortWith compareElo coaches

        EloDesc ->
            List.sortWith (reverse compareName) coaches

        Season ->
            List.sortWith (secondarySortElo compareSeason) coaches

        SeasonDesc ->
            List.sortWith (secondarySortElo <| reverse compareSeason) coaches


pageSize : Int
pageSize =
    20


pageOfList : Int -> List a -> List a
pageOfList page list =
    list
        |> drop (pageSize * page)
        |> take pageSize


lastPage : WebData (List a) -> Int
lastPage list =
    case list of
        RemoteData.Success l ->
            length l // pageSize

        _ ->
            0



-- View --


view : Model -> Html Msg
view model =
    row []
        [ viewRefreshButton
        , viewErrorMessage model.deleteError
        , viewCoachesOrError model
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    optionButton [ onClick FetchCoaches, floatRight ] [ text "Refresh Coaches" ]


viewCoachesOrError : Model -> Html Msg
viewCoachesOrError model =
    case model.coaches of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            emphasisText [] [ text "Loading..." ]

        RemoteData.Success coaches ->
            viewCoaches model.session model.sortingMethod model.page coaches

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


viewLoadError : String -> Html Msg
viewLoadError errorMessage =
    errorText []
        [ emphasisText [] [ text "Couldn't fetch data at this time." ]
        , text <| "Error: " ++ errorMessage
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just m ->
            errorText [] [ text <| "Error: " ++ m ]

        Nothing ->
            text ""


viewCoaches : Session -> SortingMethod -> Int -> List Coach -> Html Msg
viewCoaches session sortMethod page coaches =
    div []
        [ viewHeader session
        , table []
            [ viewTableHeader session sortMethod
            , tableBody []
                (sortedCoaches sortMethod coaches
                    |> pageOfList page
                    |> List.map (viewCoach session)
                )
            ]
        , viewPageSelect page (length coaches)
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    row []
        [ mainHeader [] [ text "Coaches" ]
        , requiresAuth session viewAddButton
        ]


viewAddButton : Html Msg
viewAddButton =
    addButton
        [ onClick AddCoachButtonClick, floatRight ]
        [ text "Add Coach" ]


viewTableHeader : Session -> SortingMethod -> Html Msg
viewTableHeader session sortMethod =
    tableHead []
        [ ( [ onClick NameSortClick ]
          , [ case sortMethod of
                Name ->
                    text "Name ▲"

                NameDesc ->
                    text "Name ▼"

                _ ->
                    text "Name"
            ]
          )
        , ( [ onClick SeasonSortClick ]
          , [ case sortMethod of
                Season ->
                    text "Last Played ▲"

                SeasonDesc ->
                    text "Last Played ▼"

                _ ->
                    text "Last Played"
            ]
          )
        , ( [ onClick EloSortClick ]
          , [ case sortMethod of
                Elo ->
                    text "Elo ▲"

                EloDesc ->
                    text "Elo ▼"

                _ ->
                    text "Elo"
            ]
          )
        , ( [], [ requiresAuth session <| text " " ] )
        ]


viewCoach : Session -> Coach -> Html Msg
viewCoach session coach =
    tableRow []
        [ ( []
          , [ pageLink
                [ onClick <| ViewCoachClick coach.id ]
                [ text coach.name ]
            , viewAccolades coach
            ]
          )
        , ( [], [ text <| viewRecentSeason coach ] )
        , ( [], [ text <| String.fromInt coach.elo ] )
        , ( []
          , [ requiresAuth session <| viewEditButton coach
            , requiresAuth session <| viewDeleteButton coach
            ]
          )
        ]


viewRecentSeason : Coach -> String
viewRecentSeason coach =
    case coach.recentSeason of
        Nothing ->
            ""

        Just season ->
            "Season " ++ String.fromInt season


viewAccolades : Coach -> Html Msg
viewAccolades coach =
    accoladeCollection []
        (List.sortWith (\a b -> compare (Maybe.withDefault 0 b.season) (Maybe.withDefault 0 a.season)) coach.accolades
            |> List.take 5
            |> List.map viewAccolade
        )


viewDeleteButton : Coach -> Html Msg
viewDeleteButton coach =
    warnButton
        [ onClick (DeleteCoachButtonClick coach.id) ]
        [ text "Delete" ]


viewEditButton : Coach -> Html Msg
viewEditButton coach =
    optionButton
        [ onClick (EditCoachButtonClick coach.id) ]
        [ text "Edit" ]


viewPageSelect : Int -> Int -> Html Msg
viewPageSelect page count =
    if count <= pageSize then
        text ""

    else
        pageBar []
            [ pageBarButton [ onClick FirstPageClick ] [ text "<<" ]
            , pageBarButton [ onClick PrevPageClick ] [ text "<" ]
            , pageBarFiller [] [ text <| String.fromInt (page + 1) ++ " of " ++ String.fromInt (count // pageSize + 1) ]
            , pageBarButton [ onClick NextPageClick ] [ text ">" ]
            , pageBarButton [ onClick LastPageClick ] [ text ">>" ]
            ]
