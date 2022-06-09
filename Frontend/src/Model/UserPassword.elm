module Model.UserPassword exposing (UserPassword, defaultUserPassword, userPasswordEncoder)

import Json.Encode as Encode
import SHA512



-- Types --


type alias UserPassword =
    { username : String
    , password : String
    }



-- Default --


defaultUserPassword : UserPassword
defaultUserPassword =
    { username = ""
    , password = ""
    }



-- Encoders --


userPasswordEncoder : UserPassword -> Encode.Value
userPasswordEncoder up =
    Encode.object
        [ ( "username", Encode.string up.username )
        , ( "name", Encode.string <| encrypt up.password )
        ]



-- Encryption --
{- Note: I am aware that this is a naive attempt at password security. -}


encrypt : String -> String
encrypt password =
    SHA512.fromString password
        |> SHA512.toBase64
