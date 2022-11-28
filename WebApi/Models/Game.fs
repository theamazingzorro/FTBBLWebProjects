namespace ftbbl.WebApi.Models

open NPoco


[<CLIMutable>]
type Game =
    {
        Id : int

        HomeScore : System.Nullable<int>
        AwayScore : System.Nullable<int>
        Week : int

        [<Reference(ReferenceType.Foreign, ColumnName = "div_id", ReferenceMemberName = "Id")>]
        Division : Division

        [<Reference(ReferenceType.Foreign, ColumnName = "home_team_id", ReferenceMemberName = "Id")>]
        HomeTeam : Team
        [<Reference(ReferenceType.Foreign, ColumnName = "away_team_id", ReferenceMemberName = "Id")>]
        AwayTeam : Team
    }