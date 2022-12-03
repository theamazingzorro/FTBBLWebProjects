namespace ftbbl.WebApi.Models

open NPoco


[<CLIMutable>]
type Team =
    {
        Id : int
        Name : string
        Elo : int

        [<Reference(ReferenceType.Foreign, ColumnName = "race_id", ReferenceMemberName = "Id")>]
        Race : Race
        [<Reference(ReferenceType.Foreign, ColumnName = "coach_id", ReferenceMemberName = "Id")>]
        Coach : Coach

        [<ResultColumn>]
        [<ComplexMapping>]
        Division : Division
    }