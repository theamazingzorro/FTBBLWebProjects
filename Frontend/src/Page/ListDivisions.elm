module Page.ListDivisions exposing (Model, Msg, init, update, view)

import Api
import Auth exposing (requiresAuth)
import Custom.Attributes exposing (textCentered)
import Error
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import List exposing (drop, length, take)
import Model.DeleteResponse exposing (DeleteResponse, deleteResponseDecoder)
import Model.Division exposing (Division, DivisionId, compareDivisions, divisionsDecoder)
import Model.Session exposing (Session)
import RemoteData exposing (WebData)
import Route exposing (pushUrl)
import String exposing (toLower)



-- Types --


type alias Model =
    { divisions : WebData (List Division)
    , page : Int
    , sortingMethod : SortingMethod
    , session : Session
    , deleteError : Maybe String
    }


type Msg
    = FetchDivisions
    | DivisionsRecieved (WebData (List Division))
    | AddDivisionButtonClick
    | EditDivisionButtonClick DivisionId
    | CloseDivisionButtonClick DivisionId
    | DeleteDivisionButtonClick DivisionId
    | DivisionDeleted (Result Http.Error DeleteResponse)
    | DivisionClosed (Result Http.Error String)
    | ViewDivisionButtonClick DivisionId
    | NameSortClick
    | SeasonSortClick
    | FirstPageClick
    | PrevPageClick
    | NextPageClick
    | LastPageClick


type SortingMethod
    = Default
    | Name
    | NameDesc
    | Season
    | SeasonDesc



-- Init --


init : Session -> ( Model, Cmd Msg )
init session =
    ( { divisions = RemoteData.Loading
      , page = 0
      , sortingMethod = Default
      , session = session
      , deleteError = Nothing
      }
    , getDivisionsRequest session.token
    )



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDivisions ->
            ( { model | divisions = RemoteData.Loading }, getDivisionsRequest model.session.token )

        DivisionsRecieved response ->
            ( { model | divisions = response }, Cmd.none )

        AddDivisionButtonClick ->
            ( model, pushUrl model.session.navkey Route.AddDivision )

        EditDivisionButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.EditDivision id )

        ViewDivisionButtonClick id ->
            ( model, pushUrl model.session.navkey <| Route.ViewDivision id )

        DeleteDivisionButtonClick id ->
            ( model, deleteDivisionRequest model.session.token id )

        CloseDivisionButtonClick id ->
            ( { model | deleteError = Nothing }, closeDivRequest model.session.token id )

        DivisionDeleted (Ok res) ->
            ( { model | deleteError = buildDeleteError res }, getDivisionsRequest model.session.token )

        DivisionDeleted (Err err) ->
            ( { model | deleteError = Just (Error.buildErrorMessage err) }, Cmd.none )

        DivisionClosed (Ok _) ->
            ( { model | deleteError = Nothing }, getDivisionsRequest model.session.token )

        DivisionClosed (Err err) ->
            ( { model | deleteError = Just (Error.buildErrorMessage err) }, Cmd.none )

        NameSortClick ->
            ( { model | sortingMethod = newSort Name NameDesc model.sortingMethod }, Cmd.none )

        SeasonSortClick ->
            ( { model | sortingMethod = newSort Season SeasonDesc model.sortingMethod }, Cmd.none )

        FirstPageClick ->
            ( { model | page = 0 }, Cmd.none )

        PrevPageClick ->
            ( { model | page = Basics.max 0 (model.page - 1) }, Cmd.none )

        NextPageClick ->
            ( { model | page = Basics.min (lastPage model.divisions) (model.page + 1) }, Cmd.none )

        LastPageClick ->
            ( { model | page = lastPage model.divisions }, Cmd.none )


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
        Just "Delete Failed. If this div ever had teams assigned it cannot be deleted."



-- API Requests --


getDivisionsRequest : Maybe String -> Cmd Msg
getDivisionsRequest token =
    Api.getRequest token Api.Divisions <|
        Http.expectJson (RemoteData.fromResult >> DivisionsRecieved) divisionsDecoder


deleteDivisionRequest : Maybe String -> DivisionId -> Cmd Msg
deleteDivisionRequest token id =
    Api.deleteRequest token (Api.Division id) <|
        Http.expectJson DivisionDeleted deleteResponseDecoder


closeDivRequest : Maybe String -> DivisionId -> Cmd Msg
closeDivRequest token divId =
    Api.postRequest token (Api.CloseDivision divId) Http.emptyBody <|
        Http.expectString DivisionClosed



-- Helper Functions --


sortedDivs : SortingMethod -> List Division -> List Division
sortedDivs sortingMethod divs =
    let
        compareName a b =
            compareStrIgnoreCase a.name b.name

        compareSeason a b =
            compare a.season b.season

        compareStrIgnoreCase a b =
            compare (toLower a) (toLower b)

        reverse func a b =
            func b a
    in
    case sortingMethod of
        Default ->
            List.sortWith (reverse compareDivisions) divs

        Name ->
            List.sortWith compareName divs

        NameDesc ->
            List.sortWith (reverse compareName) divs

        Season ->
            List.sortWith compareSeason divs

        SeasonDesc ->
            List.sortWith (reverse compareSeason) divs


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
        , viewDivisionsOrError model
        ]


viewHeader : Session -> Html Msg
viewHeader session =
    header [ class "main" ]
        [ div [ class "row" ]
            [ div [ class "col-10" ]
                [ h1 [] [ text "Divisions" ] ]
            , div [ class "col-2" ]
                [ requiresAuth session viewAddButton ]
            ]
        ]


viewAddButton : Html Msg
viewAddButton =
    a
        [ href "#"
        , class "button primary"
        , onClick AddDivisionButtonClick
        ]
        [ text "Add Division" ]


viewRefreshButton : Html Msg
viewRefreshButton =
    div [ class "row" ]
        [ div [ class "col-10" ] []
        , div [ class "col-2" ]
            [ a
                [ onClick FetchDivisions
                , href "#"
                , class "button small"
                ]
                [ text "Refresh Divs" ]
            ]
        ]


viewDivisionsOrError : Model -> Html Msg
viewDivisionsOrError model =
    case model.divisions of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            h3 [] [ text "Loading..." ]

        RemoteData.Success divisions ->
            viewDivisions model.session model.sortingMethod model.page divisions

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


viewDivisions : Session -> SortingMethod -> Int -> List Division -> Html Msg
viewDivisions session sortMethod page divisions =
    div [ class "table-wrapper" ]
        [ table [ Custom.Attributes.table ]
            [ viewTableHeader session sortMethod
            , sortedDivs sortMethod divisions
                |> pageOfList page
                |> List.map (viewDivision session)
                |> tbody []
            ]
        , viewPageSelect page (length divisions)
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
                        text "Season ▲"

                    SeasonDesc ->
                        text "Season ▼"

                    _ ->
                        text "Season"
                ]
            , th [ scope "col" ]
                [ text "Open?" ]
            , requiresAuth session <|
                th [ scope "col" ]
                    [ text "" ]
            ]
        ]


viewDivision : Session -> Division -> Html Msg
viewDivision session division =
    tr []
        [ td []
            [ a [ href "#", onClick <| ViewDivisionButtonClick division.id ]
                [ text division.name ]
            ]
        , td [ textCentered ]
            [ text <| String.fromInt division.season ]
        , td []
            [ if division.closed then
                text "Closed"

              else
                text "Ongoing"
            ]
        , requiresAuth session <|
            td []
                [ viewAdminButtons division
                ]
        ]


viewAdminButtons : Division -> Html Msg
viewAdminButtons division =
    ul [ class "actions small stacked" ]
        [ li [] [ viewCloseButton division ]
        , li [] [ viewEditButton division ]
        , li [] [ viewDeleteButton division ]
        ]


viewDeleteButton : Division -> Html Msg
viewDeleteButton division =
    a
        [ onClick <| DeleteDivisionButtonClick division.id
        , href "#"
        , class "button small"
        ]
        [ text "Delete" ]


viewEditButton : Division -> Html Msg
viewEditButton division =
    a
        [ onClick <| EditDivisionButtonClick division.id
        , href "#"
        , class "button primary small"
        ]
        [ text "Edit" ]


viewCloseButton : Division -> Html Msg
viewCloseButton division =
    if division.closed then
        text ""

    else
        a
        [ onClick <| CloseDivisionButtonClick division.id
        , href "#"
        , class "button primary small"
        ]
        [ text "Close" ]


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
