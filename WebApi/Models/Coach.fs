namespace ftbbl.WebApi.Models

open NPoco

[<CLIMutable>]
type Coach =
    {
        Id : int
        Name : string
        Elo : int

        [<Ignore>]
        Accolades : Accolade list
    }


