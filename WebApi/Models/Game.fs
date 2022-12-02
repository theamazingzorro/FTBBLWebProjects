namespace ftbbl.WebApi.Models

open NPoco


[<CLIMutable>]
type Game =
    {
        Id : int

        [<Column(Name = "home_score")>]
        HomeScore : System.Nullable<int>
        [<Column(Name = "away_score")>]
        AwayScore : System.Nullable<int>

        [<Ignore>]
        HomeOdds : System.Nullable<float>
        [<Ignore>]
        AwayOdds : System.Nullable<float>

        Week : int

        [<Reference(ReferenceType.Foreign, ColumnName = "div_id", ReferenceMemberName = "Id")>]
        Division : Division

        [<Reference(ReferenceType.Foreign, ColumnName = "home_team_id", ReferenceMemberName = "Id")>]
        HomeTeam : Team
        [<Reference(ReferenceType.Foreign, ColumnName = "away_team_id", ReferenceMemberName = "Id")>]
        AwayTeam : Team
    }