namespace ftbbl.WebApi.Models

open NPoco

[<CLIMutable>]
type Division =
    {
        Id : int
        Name : string
        Season : uint
        Closed : bool

        [<Column(Name = "league_id")>]
        LeagueId : int
    }

