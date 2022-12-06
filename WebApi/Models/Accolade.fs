namespace ftbbl.WebApi.Models

open NPoco


[<CLIMutable>]
type Accolade =
    {
        Id : int

        [<Column(Name = "team_id")>]
        TeamId : System.Nullable<int>
        [<Column(Name = "coach_id")>]
        CoachId : int

        Season : System.Nullable<int>
        Name : string

        [<Column(Name = "championship")>]
        IsChamp : bool
        [<Column(Name = "runnerup")>]
        IsRunnerup : bool
        [<Column(Name = "sidecup")>]
        IsSidecup : bool
    }