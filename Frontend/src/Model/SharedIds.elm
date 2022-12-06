module Model.SharedIds exposing
    ( CoachId
    , TeamId
    , coachIdDecoder
    , coachIdParser
    , coachIdToString
    , defaultCoachId
    , defaultTeamId
    , encodeCoachId
    , encodeTeamId
    , teamIdDecoder
    , teamIdParser
    , teamIdToString
    )

import Json.Decode as Decode exposing (Decoder, int)
import Json.Encode as Encode
import Url.Parser exposing (Parser, custom)



-- Coach Id --


type CoachId
    = CoachId Int


coachIdToString : CoachId -> String
coachIdToString (CoachId id) =
    String.fromInt id


defaultCoachId : CoachId
defaultCoachId =
    CoachId 0


coachIdDecoder : Decoder CoachId
coachIdDecoder =
    Decode.map CoachId int


encodeCoachId : CoachId -> Encode.Value
encodeCoachId (CoachId id) =
    Encode.int id


coachIdParser : Parser (CoachId -> a) a
coachIdParser =
    custom "COACHID" <|
        \id ->
            Maybe.map CoachId (String.toInt id)



-- Team Id --


type TeamId
    = TeamId Int


teamIdToString : TeamId -> String
teamIdToString (TeamId id) =
    String.fromInt id


defaultTeamId : TeamId
defaultTeamId =
    TeamId 0


teamIdDecoder : Decoder TeamId
teamIdDecoder =
    Decode.map TeamId int


encodeTeamId : TeamId -> Encode.Value
encodeTeamId (TeamId id) =
    Encode.int id


teamIdParser : Parser (TeamId -> a) a
teamIdParser =
    custom "TEAMID" <|
        \id ->
            Maybe.map TeamId (String.toInt id)
