module Model.Race exposing
    ( Race
    , RaceId
    , defaultRace
    , raceDecoder
    , raceEncoder
    )

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode



-- Types --


type alias Race =
    { id : RaceId
    , name : String
    }


type RaceId
    = RaceId Int



-- Default --


defaultRace : Race
defaultRace =
    { id = RaceId 0
    , name = ""
    }



-- Decoders --


raceDecoder : Decoder Race
raceDecoder =
    Decode.succeed Race
        |> required "id" raceIdDecoder
        |> required "name" string


raceIdDecoder : Decoder RaceId
raceIdDecoder =
    Decode.map RaceId int



-- Encoders --


raceEncoder : Race -> Encode.Value
raceEncoder race =
    Encode.object
        [ ( "id", encodeId race.id )
        , ( "name", Encode.string race.name )
        ]


encodeId : RaceId -> Encode.Value
encodeId (RaceId id) =
    Encode.int id
