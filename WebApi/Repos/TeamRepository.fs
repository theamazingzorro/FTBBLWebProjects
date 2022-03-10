namespace ftbbl.WebApi.Repositories

module TeamRepository = 
    open ftbbl.WebApi.Models.Team

    open Microsoft.AspNetCore.Builder
    open System
    open MySql.Data.MySqlClient
    open NPoco

    let connStr = WebApplication.CreateBuilder().Configuration["ConnString"]


    let getAll() =  
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        db.Fetch<Team>("""
                SELECT * FROM Team
                JOIN Race ON Team.race_id=Race.id
                JOIN Coach ON Team.coach_id=Coach.id
                WHERE Team.is_active
                """)
            |> List.ofSeq


    let getById (id : int) =
        use connection = new MySqlConnection(connStr)
        connection.Open()

        use db = new Database(connection)

        try 
            db.Single<Team>("""
                SELECT * FROM Team
                JOIN Race ON Team.race_id=Race.id
                JOIN Coach ON Team.coach_id=Coach.id
                WHERE Team.is_active
                AND Team.id=@0""", id)
        with
            :? InvalidOperationException -> { Id=0; Name=""; Race={Id=0; Name=""}; Coach={Id=0; Name=""; Elo=0}; Elo=0 }
        
        
        
        