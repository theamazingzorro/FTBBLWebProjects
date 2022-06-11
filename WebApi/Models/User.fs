namespace ftbbl.WebApi.Models

open NPoco

[<CLIMutable>]
type User =
    {
        Id : int
        Username : string
        Password : string
        [<Column(Name = "is_admin")>]
        IsAdmin : bool
    }


