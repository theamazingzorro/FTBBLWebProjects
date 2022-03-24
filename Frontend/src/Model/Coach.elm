module Model.Coach exposing (Coach, CoachId, coachDecoder, coachEncoder, coachsDecoder, defaultCoach, idParser, idToString, newCoachEncoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)



-- Types --


type alias Coach =
    { id : CoachId
    , name : String
    , elo : Int
    }


type CoachId
    = CoachId Int



-- ToString --


idToString : CoachId -> String
idToString (CoachId id) =
    String.fromInt id



-- Default --


defaultCoach : Coach
defaultCoach =
    { id = CoachId 0
    , name = ""
    , elo = 1000
    }



-- Decoders --


coachsDecoder : Decoder (List Coach)
coachsDecoder =
    list coachDecoder


coachDecoder : Decoder Coach
coachDecoder =
    Decode.succeed Coach
        |> required "id" coachIdDecoder
        |> required "name" string
        |> required "elo" int


coachIdDecoder : Decoder CoachId
coachIdDecoder =
    Decode.map CoachId int



-- Encoders --


coachEncoder : Coach -> Encode.Value
coachEncoder coach =
    Encode.object
        [ ( "id", encodeId coach.id )
        , ( "name", Encode.string coach.name )
        , ( "elo", Encode.int coach.elo )
        ]


newCoachEncoder : Coach -> Encode.Value
newCoachEncoder coach =
    Encode.object
        [ ( "name", Encode.string coach.name )
        , ( "elo", Encode.int coach.elo )
        ]


encodeId : CoachId -> Encode.Value
encodeId (CoachId id) =
    Encode.int id



-- Parsers --


idParser : Parser (CoachId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map CoachId (String.toInt postId)
