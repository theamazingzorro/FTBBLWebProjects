namespace ftbbl.WebApi.Repositories

module TeamRepository = 
    open ftbbl.WebApi.Models

    let _teams: List<Team> = [
            {Name="The Government"; Race="Lizardmen"; Coach="Theamazingzorro"; IsActive=true}
            {Name="Scooby Snacks"; Race="Necromantic"; Coach="Danean"; IsActive=true}
        ]

    let getAll: List<Team> = 
        List.where (fun x -> x.IsActive) _teams
        