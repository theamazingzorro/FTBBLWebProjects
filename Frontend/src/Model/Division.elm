module Model.Division exposing
    ( Division
    , DivisionId
    , defaultDivision
    , divisionDecoder
    , divisionEncoder
    , divisionsDecoder
    , idParser
    , idToString
    , newDivisionEncoder
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)



-- Types --


type alias Division =
    { id : DivisionId
    , name : String
    , season : Int
    }


type DivisionId
    = DivisionId Int



-- ToString --


idToString : DivisionId -> String
idToString (DivisionId id) =
    String.fromInt id



-- Default --


defaultDivision : Division
defaultDivision =
    { id = DivisionId 0
    , name = ""
    , season = 0
    }



-- Decoders --


divisionsDecoder : Decoder (List Division)
divisionsDecoder =
    list divisionDecoder


divisionDecoder : Decoder Division
divisionDecoder =
    Decode.succeed Division
        |> required "id" divisionIdDecoder
        |> required "name" string
        |> required "season" int


divisionIdDecoder : Decoder DivisionId
divisionIdDecoder =
    Decode.map DivisionId int



-- Encoders --


divisionEncoder : Division -> Encode.Value
divisionEncoder division =
    Encode.object
        [ ( "id", encodeId division.id )
        , ( "name", Encode.string division.name )
        , ( "season", Encode.int division.season )
        ]


newDivisionEncoder : Division -> Encode.Value
newDivisionEncoder division =
    Encode.object
        [ ( "name", Encode.string division.name )
        , ( "season", Encode.int division.season )
        ]


encodeId : DivisionId -> Encode.Value
encodeId (DivisionId id) =
    Encode.int id



-- Parsers --


idParser : Parser (DivisionId -> a) a
idParser =
    custom "POSTID" <|
        \postId ->
            Maybe.map DivisionId (String.toInt postId)
