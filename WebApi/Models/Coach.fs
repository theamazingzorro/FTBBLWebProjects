namespace ftbbl.WebApi.Models

open NPoco

[<CLIMutable>]
type Coach =
    {
        Id : int
        Name : string
        Elo : int

        [<Column(Name = "league_id")>]
        LeagueId : int

        [<Ignore>]
        Accolades : Accolade list
    }


