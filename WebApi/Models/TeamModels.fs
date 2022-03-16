namespace ftbbl.WebApi.Models.Team

open NPoco

[<CLIMutable>]
type Race =
    {
        Id : int
        Name : string

        [<Column("is_active")>]
        IsActive : bool
    }

[<CLIMutable>]
type Team =
    {
        Id : int
        Name : string
        Elo : int

        [<Column("is_active")>]
        IsActive : bool

        [<Reference(ReferenceType.Foreign, ColumnName = "race_id", ReferenceMemberName = "Id")>]
        Race : Race
        [<Reference(ReferenceType.Foreign, ColumnName = "coach_id", ReferenceMemberName = "Id")>]
        Coach : Coach
    }

and [<CLIMutable>]
Coach =
    {
        Id : int
        Name : string
        Elo : int

        [<Column("is_active")>]
        IsActive : bool
    }


