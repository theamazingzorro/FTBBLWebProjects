module Model.Accolade exposing
    ( Accolade
    , AccoladeId
    , accoladeDecoder
    , accoladeEncoder
    , accoladesDecoder
    , defaultAccolade
    , idParser
    , idToString
    , newAccoladeEncoder
    , viewAccolade
    )

import Html exposing (Html, span, text)
import Html.Attributes exposing (title)
import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Model.SharedIds exposing (..)
import Url.Parser exposing (Parser, custom)



-- Types --


type alias Accolade =
    { id : AccoladeId
    , teamId : Maybe TeamId
    , coachId : CoachId
    , season : Maybe Int
    , name : String
    , isChamp : Bool
    , isRunnerUp : Bool
    , isSidecup : Bool
    }


type AccoladeId
    = AccoladeId Int



-- ToString --


idToString : AccoladeId -> String
idToString (AccoladeId id) =
    String.fromInt id



-- Default --


defaultAccolade : Accolade
defaultAccolade =
    { id = AccoladeId 0
    , teamId = Nothing
    , coachId = defaultCoachId
    , season = Nothing
    , name = ""
    , isChamp = False
    , isRunnerUp = False
    , isSidecup = False
    }



-- Utils --


viewAccolade : Accolade -> Html msg
viewAccolade accolade =
    span [ title accolade.name ]
        [ if accolade.isChamp then
            text "🥇"

          else if accolade.isRunnerUp then
            text "🥈"

          else if accolade.isSidecup then
            text "🏆"

          else
            text "🏅"
        ]



-- Decoders --


accoladesDecoder : Decoder (List Accolade)
accoladesDecoder =
    list accoladeDecoder


accoladeDecoder : Decoder Accolade
accoladeDecoder =
    Decode.succeed Accolade
        |> required "id" accoladeIdDecoder
        |> optional "teamId" (Decode.map Just teamIdDecoder) Nothing
        |> required "coachId" coachIdDecoder
        |> optional "season" (Decode.map Just int) Nothing
        |> required "name" string
        |> required "isChamp" bool
        |> required "isRunnerup" bool
        |> required "isSidecup" bool


accoladeIdDecoder : Decoder AccoladeId
accoladeIdDecoder =
    Decode.map AccoladeId int



-- Encoders --


accoladeEncoder : Accolade -> Encode.Value
accoladeEncoder accolade =
    Encode.object
        [ ( "id", encodeId accolade.id )
        , ( "teamId", encodeMaybe encodeTeamId accolade.teamId )
        , ( "coachId", encodeCoachId accolade.coachId )
        , ( "season", encodeMaybe Encode.int accolade.season )
        , ( "name", Encode.string accolade.name )
        , ( "isChamp", Encode.bool accolade.isChamp )
        , ( "isRunnerup", Encode.bool accolade.isRunnerUp )
        , ( "isSidecup", Encode.bool accolade.isSidecup )
        ]


newAccoladeEncoder : Accolade -> Encode.Value
newAccoladeEncoder accolade =
    Encode.object
        [ ( "teamId", encodeMaybe encodeTeamId accolade.teamId )
        , ( "coachId", encodeCoachId accolade.coachId )
        , ( "season", encodeMaybe Encode.int accolade.season )
        , ( "name", Encode.string accolade.name )
        , ( "isChamp", Encode.bool accolade.isChamp )
        , ( "isRunnerup", Encode.bool accolade.isRunnerUp )
        , ( "isSidecup", Encode.bool accolade.isSidecup )
        ]


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encoding maybeVal =
    maybeVal
        |> Maybe.map encoding
        |> Maybe.withDefault Encode.null


encodeId : AccoladeId -> Encode.Value
encodeId (AccoladeId id) =
    Encode.int id



-- Parsers --


idParser : Parser (AccoladeId -> a) a
idParser =
    custom "AccoladeId" <|
        \id ->
            Maybe.map AccoladeId (String.toInt id)
