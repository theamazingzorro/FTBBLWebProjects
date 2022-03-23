module Model.DeleteResponse exposing (..)

import Json.Decode as Decode exposing (Decoder, bool)
import Json.Decode.Pipeline exposing (required)


type alias DeleteResponse =
    { deleted : Bool
    }


deleteResponseDecoder : Decoder DeleteResponse
deleteResponseDecoder =
    Decode.succeed DeleteResponse
        |> required "deleted" bool
