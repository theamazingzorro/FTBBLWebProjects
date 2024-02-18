namespace ftbbl.WebApi.Models

open NPoco
open System

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

        [<ResultColumn(Name = "recent_season")>]
        RecentSeason : Nullable<int>
    }


