module Model.Division exposing
    ( Division
    , DivisionId
    , compareDivisions
    , defaultDivision
    , divisionDecoder
    , divisionEncoder
    , divisionIdDecoder
    , divisionsDecoder
    , idParser
    , idToString
    , newDivisionEncoder
    )

import Json.Decode as Decode exposing (Decoder, bool, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)



-- Types --


type alias Division =
    { id : DivisionId
    , name : String
    , season : Int
    , closed : Bool
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
    , closed = False
    }



-- Utils --


compareDivisions : Division -> Division -> Order
compareDivisions a b =
    case compare a.season b.season of
        EQ ->
            compare a.name b.name

        other ->
            other



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
        |> required "closed" bool


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
        , ( "closed", Encode.bool division.closed )
        ]


newDivisionEncoder : Division -> Encode.Value
newDivisionEncoder division =
    Encode.object
        [ ( "name", Encode.string division.name )
        , ( "season", Encode.int division.season )
        , ( "closed", Encode.bool division.closed )
        ]


encodeId : DivisionId -> Encode.Value
encodeId (DivisionId id) =
    Encode.int id



-- Parsers --


idParser : Parser (DivisionId -> a) a
idParser =
    custom "DIVID" <|
        \id ->
            Maybe.map DivisionId (String.toInt id)
