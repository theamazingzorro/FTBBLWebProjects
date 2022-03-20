module Model.Coach exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Coach =
    { id : CoachId
    , name : String
    , elo : Int
    }


type alias CoachId =
    Int


coachIdFromInt : Int -> CoachId
coachIdFromInt i =
    i


coachIdToInt : CoachId -> Int
coachIdToInt i =
    i


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
    Decode.map coachIdFromInt int


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
encodeId coachId =
    Encode.int coachId
