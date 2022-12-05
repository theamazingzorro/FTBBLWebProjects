module Model.Coach exposing
    ( Coach
    , CoachId
    , coachDecoder
    , coachEncoder
    , coachIdDecoder
    , coachsDecoder
    , defaultCoach
    , encodeId
    , idParser
    , idToString
    , newCoachEncoder
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Model.SharedIds as SharedIds
import Url.Parser exposing (Parser)



-- Types --


type alias Coach =
    { id : CoachId
    , name : String
    , elo : Int
    }


type alias CoachId =
    SharedIds.CoachId



-- ToString --


idToString : CoachId -> String
idToString =
    SharedIds.coachIdToString



-- Default --


defaultCoach : Coach
defaultCoach =
    { id = SharedIds.defaultCoachId
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
    SharedIds.coachIdDecoder



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
encodeId =
    SharedIds.encodeCoachId



-- Parsers --


idParser : Parser (CoachId -> a) a
idParser =
    SharedIds.coachIdParser
