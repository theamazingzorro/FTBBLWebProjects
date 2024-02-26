module Page.ListCoaches exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Attributes exposing (textCentered)
import Error exposing (buildErrorMessage)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    section []
        [ viewHeader model.session
        , viewRefreshButton
        , hr [ class "major" ] []
        , viewErrorMessage model.deleteError
        , viewCoachesOrError model
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    header [ class "main" ]
        [ div [ class "row" ]
            [ div [ class "col-10" ]
                [ h1 [] [ text "Coaches" ] ]
            , div [ class "col-2" ]
                [ requiresAuth session viewAddButton ]
            ]
        ]


viewAddButton : Html Msg
viewAddButton =
    a
        [ href "#"
        , class "button primary"
        , onClick AddCoachButtonClick
        ]
        [ text "Add Coach" ]


viewRefreshButton : Html Msg
viewRefreshButton =
    div [ class "row" ]
        [ div [ class "col-10" ] []
        , div [ class "col-2" ]
            [ a
                [ onClick FetchCoaches
                , href "#"
                , class "button small"
                ]
                [ text "Refresh Coaches" ]
            ]
        ]


viewCoachesOrError : Model -> Html Msg
viewCoachesOrError model =
    case model.coaches of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success coaches ->
            viewCoaches model.session model.sortingMethod model.page coaches

        RemoteData.Failure httpError ->
            viewLoadError <| Error.buildErrorMessage httpError


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


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage message =
    case message of
        Just m ->
            div [ Custom.Attributes.errorMessage ]
                [ text <| "Error: " ++ m ]

        Nothing ->
            text ""


viewCoaches : Session -> SortingMethod -> Int -> List Coach -> Html Msg
viewCoaches session sortMethod page coaches =
    div [ class "table-wrapper" ]
        [ table [ Custom.Attributes.table ]
            [ viewTableHeader session sortMethod
            , sortedCoaches sortMethod coaches
                |> pageOfList page
                |> List.map (viewCoach session)
                |> tbody []
            ]
        , viewPageSelect page (length coaches)
        ]


viewTableHeader : Session -> SortingMethod -> Html Msg
viewTableHeader session sortMethod =
    thead []
        [ tr []
            [ th [ scope "col", onClick NameSortClick ]
                [ case sortMethod of
                    Name ->
                        text "Name ▲"

                    NameDesc ->
                        text "Name ▼"

                    _ ->
                        text "Name"
                ]
            , th [ scope "col", onClick SeasonSortClick, textCentered ]
                [ case sortMethod of
                    Season ->
                        text "Last Played ▲"

                    SeasonDesc ->
                        text "Last Played ▼"

                    _ ->
                        text "Last Played"
                ]
            , th [ scope "col", onClick EloSortClick, textCentered ]
                [ case sortMethod of
                    Elo ->
                        text "Elo ▲"

                    EloDesc ->
                        text "Elo ▼"

                    _ ->
                        text "Elo"
                ]
            , requiresAuth session <|
                th [ scope "col" ]
                    [ text "" ]
            ]
        ]


viewCoach : Session -> Coach -> Html Msg
viewCoach session coach =
    tr []
        [ td []
            [ a [ href "#", onClick <| ViewCoachClick coach.id ]
                [ text coach.name ]
            , viewAccolades coach
            ]
        , td [ textCentered ]
            [ text <| viewRecentSeason coach ]
        , td [ textCentered ]
            [ text <| String.fromInt coach.elo ]
        , requiresAuth session <|
            td []
                [ viewAdminButtons coach ]
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
    span []
        (List.sortWith (\a b -> compare (Maybe.withDefault 0 b.season) (Maybe.withDefault 0 a.season)) coach.accolades
            |> List.take 5
            |> List.map viewAccolade
        )


viewAdminButtons : Coach -> Html Msg
viewAdminButtons coach =
    ul [ class "actions small stacked" ]
        [ li [] [ viewEditButton coach ]
        , li [] [ viewDeleteButton coach ]
        ]


viewDeleteButton : Coach -> Html Msg
viewDeleteButton coach =
    a
        [ onClick <| DeleteCoachButtonClick coach.id
        , href "#"
        , class "button small"
        ]
        [ text "Delete" ]


viewEditButton : Coach -> Html Msg
viewEditButton coach =
    a
        [ onClick <| EditCoachButtonClick coach.id
        , href "#"
        , class "button primary small"
        ]
        [ text "Edit" ]


viewPageSelect : Int -> Int -> Html Msg
viewPageSelect page teamsCount =
    if teamsCount <= pageSize then
        text ""

    else
        div [ class "row" ]
            [ div [ class "col-4" ] []
            , div [ class "col-4" ]
                [ ul [ class "actions" ]
                    [ li [] [ a [ href "#", class "button", onClick FirstPageClick ] [ text "<<" ] ]
                    , li [] [ a [ href "#", class "button", onClick PrevPageClick ] [ text "<" ] ]
                    , li [] [ text <| String.fromInt (page + 1) ++ " of " ++ String.fromInt (teamsCount // pageSize + 1) ]
                    , li [] [ a [ href "#", class "button", onClick NextPageClick ] [ text ">" ] ]
                    , li [] [ a [ href "#", class "button", onClick LastPageClick ] [ text ">>" ] ]
                    ]
                ]
            ]
