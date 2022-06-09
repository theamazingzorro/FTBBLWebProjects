module Model.Session exposing (Session, defaultSession)

import Browser.Navigation as Nav



-- Models --


type alias Session =
    { token : Maybe String
    , navkey : Nav.Key
    }



-- Default --


defaultSession : Nav.Key -> Session
defaultSession navkey =
    { token = Nothing
    , navkey = navkey 
    }
